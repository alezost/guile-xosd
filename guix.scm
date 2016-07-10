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
;; release and for the latest (more or less) development snapshot (i.e.,
;; for one of the latest commits of the guile-xosd git repository).  To
;; build (install), run:
;;
;;   guix build --file=guix.scm
;;   guix package --install-from-file=guix.scm
;;
;; (this will build/install the development package).

;;; Code:

(use-modules
 (guix packages)
 (guix download)
 (guix git-download)
 (guix licenses)
 (guix build-system gnu)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages man)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages xdisorg)
 (gnu packages xorg))

(define guile-xosd
  (package
    (name "guile-xosd")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jrz4gkhgzbs7244583f0an4ynf7z9gninqj4xpspwq47463ifzy"))))
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

(define guile-xosd-devel
  (let ((revision "1")
        (commit "e7345d03b409e5b0d4a9868a46db2fed0521dbc4"))
    (package
      (inherit guile-xosd)
      (version (string-append (package-version guile-xosd)
                              "-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://github.com/alezost/guile-xosd.git")
                      (commit commit)))
                (file-name (string-append (package-name guile-xosd)
                                          "-" version "-checkout"))
                (sha256
                 (base32
                  "0rn00icq85nbcvvgmqclf9l656srczkhzj83c527lcj6b28iprvp"))))
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
