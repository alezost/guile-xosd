;;; xosd.scm --- Guile-XOSD procedures

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

;; This module provides more "Schemey" procedures than the ones from
;; (xosd bindings) module.

;;; Code:

(define-module (xosd)
  #:use-module (xosd bindings))

(define-public make-osd xosd-create)
(define-public kill-osd xosd-destroy)
(define-public show-osd xosd-show)
(define-public hide-osd xosd-hide)
(define-public scroll-osd xosd-scroll)
(define-public display-string-in-osd xosd-display-string)
(define-public display-percentage-in-osd xosd-display-percentage)
(define-public display-slider-in-osd xosd-display-slider)
(define-public osd-color xosd-get-colour)
(define-public osd-number-of-lines xosd-get-number-lines)
(define-public osd-on-screen? xosd-onscreen?)
(define-public osd-displayed? xosd-onscreen?)
(define-public set-osd-align! xosd-set-align!)
(define-public set-osd-position! xosd-set-pos!)
(define-public set-osd-bar-length! xosd-set-bar-length!)
(define-public set-osd-timeout! xosd-set-timeout!)
(define-public set-osd-color! xosd-set-colour!)
(define-public set-osd-font! xosd-set-font!)
(define-public set-osd-horizontal-offset! xosd-set-horizontal-offset!)
(define-public set-osd-vertical-offset! xosd-set-vertical-offset!)
(define-public set-osd-outline-offset! xosd-set-outline-offset!)
(define-public set-osd-shadow-offset! xosd-set-shadow-offset!)
(define-public set-osd-outline-color! xosd-set-outline-colour!)
(define-public set-osd-shadow-color! xosd-set-shadow-colour!)
(define-public wait-while-osd-displayed xosd-wait-until-no-display)
(define-public wait-while-osd-on-screen xosd-wait-until-no-display)

;;; xosd.scm ends here
