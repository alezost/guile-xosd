;;; xosd.scm --- Guile bindings for libxosd

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

;; This Guile module provides bindings for the libxosd C library.

;;; Code:

(define-module (xosd)
  #:use-module (xosd config))

(dynamic-call "init_xosd" (dynamic-link %library-file-name))

;;; xosd.scm ends here
