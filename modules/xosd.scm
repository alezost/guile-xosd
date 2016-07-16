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
  #:use-module (xosd bindings)
  #:export (make-osd
            display-string-in-osd
            display-percentage-in-osd
            display-slider-in-osd))

(define-public kill-osd xosd-destroy)
(define-public show-osd xosd-show)
(define-public hide-osd xosd-hide)
(define-public scroll-osd xosd-scroll)
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

(define* (make-osd #:key (lines 1) align position
                   bar-length timeout color font
                   horizontal-offset vertical-offset
                   outline-offset shadow-offset
                   outline-color shadow-color)
  "Create and return a new OSD object with the specified parameters.
LINES is the number of lines that the OSD object can display.
See 'set-osd-...' procedures for the meaning of the other arguments."
  (let ((osd (xosd-create lines)))
    (when align
      (set-osd-align! osd align))
    (when position
      (set-osd-position! osd position))
    (when bar-length
      (set-osd-bar-length! osd bar-length))
    (when timeout
      (set-osd-timeout! osd timeout))
    (when color
      (set-osd-color! osd color))
    (when font
      (set-osd-font! osd font))
    (when horizontal-offset
      (set-osd-horizontal-offset! osd horizontal-offset))
    (when vertical-offset
      (set-osd-vertical-offset! osd vertical-offset))
    (when outline-offset
      (set-osd-outline-offset! osd outline-offset))
    (when shadow-offset
      (set-osd-shadow-offset! osd shadow-offset))
    (when outline-color
      (set-osd-outline-color! osd outline-color))
    (when shadow-color
      (set-osd-shadow-color! osd shadow-color))
    osd))

(define* (display-string-in-osd osd string
                                #:optional (line-number 0))
  "Display STRING in OSD.
See 'xosd-display-string' for details."
  (xosd-display-string osd line-number string))

(define* (display-percentage-in-osd osd percentage
                                    #:optional (line-number 0))
  "Display PERCENTAGE in OSD.
See 'xosd-display-percentage' for details."
  (xosd-display-percentage osd line-number percentage))

(define* (display-slider-in-osd osd percentage
                                #:optional (line-number 0))
  "Display slider PERCENTAGE in OSD.
See 'xosd-display-slider' for details."
  (xosd-display-slider osd line-number percentage))

;;; xosd.scm ends here
