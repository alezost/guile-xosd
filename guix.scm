;;; guix.scm --- Guix package for Guile-XOSD

;; Copyright © 2016 Alex Kost <alezost@gmail.com>

;; This file is part of Guile-XOSD.

;; Guile-XOSD is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Guile-XOSD is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Guile-XOSD.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains 2 Guix packages for Guile-XOSD: for the latest
;; release and for the development snapshot (i.e., for the current
;; commit of the git checkout).  To build or install, run:
;;
;;   guix build --file=guix.scm
;;   guix package --install-from-file=guix.scm
;;
;; (this will build/install the development package using the current
;; git checkout directory).

;; Also you can use this file to make a development environment for
;; building Guile-XOSD:
;;
;;   guix environment --pure --load=guix.scm
;;   ./autogen.sh
;;   ./configure
;;   make

;;; Code:

(use-modules
 (ice-9 match)
 (ice-9 popen)
 (ice-9 rdelim)
 (srfi srfi-1)
 (srfi srfi-26)
 (guix gexp)
 (guix packages)
 (guix download)
 (guix git-download)
 (guix licenses)
 (guix build utils)
 (guix build-system gnu)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages xdisorg)
 (gnu packages xorg))

(define %source-dir (dirname (current-filename)))


;;; Package for the latest release

(define guile-xosd
  (package
    (name "guile-xosd")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ri5065c16kmgrf2pysn2ymxjqi5302lhpb07wkl1jr75ym8fn8p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.0)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxinerama" ,libxinerama)
       ("xosd" ,xosd)))
    (home-page "https://github.com/alezost/guile-xosd")
    (synopsis "XOSD bindings for Guile")
    (description
     "Guile-XOSD provides Guile bindings for @code{libxosd}, the \"X On
Screen Display\" library.")
    (license gpl3+)))


;;; Git checkout and development package

;; The code for finding git files is based on
;; <https://git.dthompson.us/guile-sdl2.git/blob/HEAD:/guix.scm>.

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-port port)
      (string-trim-right output #\newline))))

(define (git-files)
  "Return a list of all git-controlled files."
  (string-split (git-output "ls-files") #\newline))

(define git-file?
  (let ((files (git-files)))
    (lambda (file stat)
      "Return #t if FILE is the git-controlled file in '%source-dir'."
      (match (stat:type stat)
        ('directory #t)
        ((or 'regular 'symlink)
         (any (cut string-suffix? <> file) files))
        (_ #f)))))

(define (current-commit)
  (git-output "log" "-n" "1" "--pretty=format:%H"))

(define guile-xosd-devel
  (let ((commit (current-commit)))
    (package
      (inherit guile-xosd)
      (version (string-append (package-version guile-xosd)
                              "-" (string-take commit 7)))
      (source (local-file %source-dir
                          #:recursive? #t
                          #:select? git-file?))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'autogen
             (lambda _ (zero? (system* "sh" "autogen.sh")))))))
      (native-inputs
       (append (package-native-inputs guile-xosd)
               `(("autoconf" ,autoconf)
                 ("automake" ,automake)
                 ("libtool" ,libtool)
                 ("texinfo" ,texinfo)))))))

guile-xosd-devel

;;; guix.scm ends here
