;;; bindings.scm --- Guile bindings for libxosd

;; Copyright © 2016–2025 Alex Kost <alezost@gmail.com>

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

(define-module (xosd bindings)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (xosd-create
            xosd-destroy
            xosd-display-string
            xosd-display-percentage
            xosd-display-slider
            xosd-hide
            xosd-show
            xosd-onscreen?
            xosd-wait-until-no-display
            xosd-set-bar-length!
            xosd-set-align!
            xosd-set-pos!
            xosd-set-timeout!
            xosd-set-font!
            xosd-set-colour!
            xosd-set-shadow-colour!
            xosd-set-outline-colour!
            xosd-set-vertical-offset!
            xosd-set-horizontal-offset!
            xosd-set-shadow-offset!
            xosd-set-outline-offset!
            xosd-get-colour
            xosd-get-number-lines
            xosd-scroll))


;;; Auxiliary code

;; XXX Maybe this should be separated into (xosd utils) module.

(define-syntax-rule (print-error format-string args ...)
  "Write some text and a newline to stderr using 'format'."
  (begin
    (format (current-error-port) format-string args ...)
    (newline)))

(define (integer->bool integer)
  (not (= integer 0)))


;; Enum definitions

(define %XOSD_percentage 0)
(define %XOSD_string 1)
;; (define %XOSD_printf 2)      ; we don't use this feature
(define %XOSD_slider 3)

(define %XOSD_top 0)
(define %XOSD_bottom 1)
(define %XOSD_middle 2)

(define %XOSD_left 0)
(define %XOSD_center 1)
(define %XOSD_right 2)


;;; XOSD library and its procedures

(define xosd-library
  (let ((lib (delay (load-foreign-library "libxosd"))))
    (lambda ()
      "Return the linked 'libxosd' library."
      (force lib))))

(define (xosd-procedure name)
  "Helper function to get pointers to libxosd procedures."
  (foreign-library-pointer (xosd-library) name))

(define xosd-create
  (let ((proc (delay (pointer->procedure
                      '* (xosd-procedure "xosd_create")
                      (list int)))))
    (lambda* (#:optional (number-of-lines 1))
      "Create and return a new OSD object with the specified maximum
NUMBER-OF-LINES that this object can display (this value cannot be
changed)."
      ((force proc) number-of-lines))))

(define xosd-destroy
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_destroy")
                      (list '*)))))
    (lambda (osd)
      "Destroy the OSD object (free up the used memory)."
      ((force proc) osd))))

(define xosd-display-string
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_display")
                      (list '* int int '*)))))
    (lambda (osd line-number string)
      "Display STRING in the LINE-NUMBER of the OSD object.
The LINE-NUMBER must be less than the number of lines set in the
call to `xosd-create'.

This procedure returns immediately, but the STRING is displayed
until the timeout limit, set by `xosd-set-timeout!'.  If blocking
is required, `xosd-wait-until-no-display' should be called after
this procedure."
      ((force proc) osd line-number %XOSD_string (string->pointer string)))))

(define-values (xosd-display-percentage
                xosd-display-slider)
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_display")
                      (list '* int int int)))))
    (values
     (lambda (osd line-number percentage)
       "Display PERCENTAGE in the LINE-NUMBER of the OSD object.
PERCENTAGE is the number between 0 and 100.
See also `xosd-display-string'."
       ((force proc) osd line-number %XOSD_percentage percentage))
     (lambda (osd line-number percentage)
       "Display slider in the LINE-NUMBER of the OSD object.
PERCENTAGE (slider position) is the number between 0 and 100.
See also `xosd-display-string'."
       ((force proc) osd line-number %XOSD_slider percentage)))))

(define xosd-hide
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_hide")
                      (list '*)))))
    (lambda (osd)
      "Hide the OSD object."
      ((force proc) osd))))

(define xosd-show
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_show")
                      (list '*)))))
    (lambda (osd)
      "Show (redisplay) the hidden OSD object."
      ((force proc) osd))))

(define xosd-onscreen?
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_is_onscreen")
                      (list '*)))))
    (lambda (osd)
      "Return `#t' if the OSD object is displayed.  Return `#f' otherwise."
      ;; TODO 'xosd_is_onscreen' can also return -1 (on failure).
      (integer->bool ((force proc) osd)))))

(define xosd-wait-until-no-display
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_wait_until_no_display")
                      (list '*)))))
    (lambda (osd)
      "Wait until the OSD object is not displayed.
See also `xosd-display-string'."
      ((force proc) osd))))

(define xosd-set-bar-length!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_bar_length")
                      (list '* int)))))
    (lambda (osd length)
      "Change the LENGTH of a slider or percentage bar of the OSD object.
Setting LENGTH to -1 reverts to the default behavior."
      ((force proc) osd length))))

(define xosd-set-pos!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_pos")
                      (list '* int)))))
    (lambda (osd position)
      "Change the vertical POSITION of the OSD object.
POSITION should be one of the following symbols: `top', `middle', or `bottom'."
      (let ((pos (case position
                   ((top)    %XOSD_top)
                   ((middle) %XOSD_middle)
                   ((bottom) %XOSD_bottom)
                   (else (begin
                           (print-error "Unknown position: ~a" position)
                           #f)))))
        (when pos
          ((force proc) osd pos))))))

(define xosd-set-align!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_align")
                      (list '* int)))))
    (lambda (osd align)
      "Change the horizontal alignment of the OSD object.
ALIGN should be one of the following symbols: `left', `center', or `right'."
      (let ((pos (case align
                   ((left)   %XOSD_left)
                   ((center) %XOSD_center)
                   ((right)  %XOSD_right)
                   (else (begin
                           (print-error "Unknown align: ~a" align)
                           #f)))))
        (when pos
          ((force proc) osd pos))))))

(define xosd-set-timeout!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_timeout")
                      (list '* int)))))
    (lambda (osd time)
      "Change the TIME (number of seconds) before the OSD object is hidden."
      ((force proc) osd time))))

(define xosd-set-font!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_font")
                      (list '* '*)))))
    (lambda (osd font)
      "Change the font of the OSD object.
FONT is a string with the XLFD full name of the new font (see
'xfontsel' program for details)."
      ((force proc) osd (string->pointer font)))))

(define xosd-set-colour!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_colour")
                      (list '* '*)))))
    (lambda (osd color)
      "Change the color of the OSD object.
COLOR is a string with the color value (e.g., `#2e8b57') or the
color name (e.g., `DeepSkyBlue')."
      ((force proc) osd (string->pointer color)))))

(define xosd-set-shadow-colour!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_shadow_colour")
                      (list '* '*)))))
    (lambda (osd color)
      "Change the color of the text shadow of the OSD object.
See `xosd-set-colour!' for the meaning of COLOR."
      ((force proc) osd (string->pointer color)))))

(define xosd-set-outline-colour!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_outline_colour")
                      (list '* '*)))))
    (lambda (osd color)
      "Change the color of the text outline of the OSD object.
See `xosd-set-colour!' for the meaning of COLOR."
      ((force proc) osd (string->pointer color)))))

(define xosd-set-vertical-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_vertical_offset")
                      (list '* int)))))
    (lambda (osd offset)
      "Change the vertical offset of the OSD object.
OFFSET is the number of pixels to offset the OSD from its default
vertical position (specified by `xosd-set-pos!')."
      ((force proc) osd offset))))

(define xosd-set-horizontal-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_horizontal_offset")
                      (list '* int)))))
    (lambda (osd offset)
      "Change the horizontal offset of the OSD object.
OFFSET is the number of pixels to offset the OSD from its default
horizontal position (specified by `xosd-set-align!')."
      ((force proc) osd offset))))

(define xosd-set-shadow-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_shadow_offset")
                      (list '* int)))))
    (lambda (osd offset)
      "Change the offset of the text shadow of the OSD object.
OFFSET is the new shadow offset in pixels."
      ((force proc) osd offset))))

(define xosd-set-outline-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_outline_offset")
                      (list '* int)))))
    (lambda (osd offset)
      "Change the offset of the text outline of the OSD object.
OFFSET is the new outline offset in pixels."
      ((force proc) osd offset))))

(define xosd-get-colour
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_get_colour")
                      (list '* '* '* '*)))))
    (lambda (osd)
      "Get the RGB value of the OSD object's color.
Return a list of 3 numbers (red, green and blue).
Note: the returned values have X11 color format i.e.,
16-bit values (0-65535), not 8-bit values (0-255)."
      (let* ((endianness (native-endianness))
             (int-size   (sizeof int))
             (red-bv     (make-bytevector int-size))
             (green-bv   (make-bytevector int-size))
             (blue-bv    (make-bytevector int-size))
             (red-ptr    (bytevector->pointer red-bv))
             (green-ptr  (bytevector->pointer green-bv))
             (blue-ptr   (bytevector->pointer blue-bv))
             (result     ((force proc) osd red-ptr green-ptr blue-ptr)))
        (if (= result 0)
            (list (bytevector-sint-ref red-bv 0   endianness int-size)
                  (bytevector-sint-ref green-bv 0 endianness int-size)
                  (bytevector-sint-ref blue-bv 0  endianness int-size))
            #f)))))

(define xosd-get-number-lines
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_get_number_lines")
                      (list '*)))))
    (lambda (osd)
      "Return the maximum number of lines allowed for the OSD object."
      ((force proc) osd))))

(define xosd-scroll
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_scroll")
                      (list '* int)))))
    (lambda (osd number-of-lines)
      "Scroll the OSD object by the NUMBER-OF-LINES."
      ((force proc) osd number-of-lines))))

;;; bindings.scm ends here
