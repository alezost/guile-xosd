;;; xosd.scm --- Guile-XOSD procedures

;; Copyright © 2016–2026 Alex Kost <alezost@gmail.com>

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
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (make-osd
            create-osd
            kill-osd
            show-osd
            hide-osd
            toggle-osd
            scroll-osd
            osd-color
            osd-number-of-lines
            osd-displayed?
            wait-while-osd-displayed
            set-osd!
            set-osd-align!
            set-osd-position!
            set-osd-bar-length!
            set-osd-timeout!
            set-osd-color!
            set-osd-font!
            set-osd-horizontal-offset!
            set-osd-vertical-offset!
            set-osd-outline-offset!
            set-osd-shadow-offset!
            set-osd-outline-color!
            set-osd-shadow-color!
            display-string-in-osd
            display-percentage-in-osd
            display-slider-in-osd))


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


;;; Wrappers for XOSD library and its procedures

(define xosd-library
  (let ((lib (delay (load-foreign-library "libxosd"))))
    (lambda ()
      "Return the linked 'libxosd' library."
      (force lib))))

(define (xosd-procedure name)
  "Helper function to get pointers to libxosd procedures."
  (foreign-library-pointer (xosd-library) name))

(define create-osd
  (let ((proc (delay (pointer->procedure
                      '* (xosd-procedure "xosd_create")
                      (list int)))))
    (lambda* (#:optional (number-of-lines 1))
      "Create and return a new OSD object with the specified maximum
NUMBER-OF-LINES that this object can display (this value cannot be
changed after OSD creation)."
      ((force proc) number-of-lines))))

(define kill-osd
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_destroy")
                      (list '*)))))
    (lambda (osd)
      "Destroy OSD object (free up the used memory)."
      ((force proc) osd))))

(define display-string-in-osd
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_display")
                      (list '* int int '*)))))
    (lambda* (osd string #:optional (line-number 0))
      "Display STRING in LINE-NUMBER of OSD object.

LINE-NUMBER must be less than the number of lines set during OSD creation.

This procedure returns immediately but STRING is displayed until timeout
set by `set-osd-timeout!' is expired.  If blocking is required,
`wait-while-osd-displayed' should be called after this procedure."
      ((force proc) osd line-number %XOSD_string (string->pointer string)))))

(define-values (display-percentage-in-osd
                display-slider-in-osd)
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_display")
                      (list '* int int int)))))
    (values
     (lambda* (osd percentage #:optional (line-number 0))
       "Display PERCENTAGE in LINE-NUMBER of OSD object.
PERCENTAGE is a number between 0 and 100.
See also `display-string-in-osd'."
       ((force proc) osd line-number %XOSD_percentage percentage))
     (lambda* (osd percentage #:optional (line-number 0))
       "Display slider in LINE-NUMBER of OSD object.
PERCENTAGE (slider position) is a number between 0 and 100.
See also `display-string-in-osd'."
       ((force proc) osd line-number %XOSD_slider percentage)))))

(define hide-osd
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_hide")
                      (list '*)))))
    (lambda (osd)
      "Hide OSD object."
      ((force proc) osd))))

(define show-osd
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_show")
                      (list '*)))))
    (lambda (osd)
      "Show (redisplay) hidden OSD object."
      ((force proc) osd))))

(define osd-displayed?
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_is_onscreen")
                      (list '*)))))
    (lambda (osd)
      "Return `#t' if OSD object is displayed.  Return `#f' otherwise."
      ;; TODO 'xosd_is_onscreen' can also return -1 (on failure).
      (integer->bool ((force proc) osd)))))

(define wait-while-osd-displayed
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_wait_until_no_display")
                      (list '*)))))
    (lambda (osd)
      "Wait until OSD object will be hidden.
See also `display-string-in-osd'."
      ((force proc) osd))))

(define set-osd-bar-length!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_bar_length")
                      (list '* int)))))
    (lambda (osd length)
      "Change LENGTH of a slider or percentage bar of OSD object.
Setting LENGTH to -1 reverts to the default behavior."
      ((force proc) osd length))))

(define set-osd-position!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_pos")
                      (list '* int)))))
    (lambda (osd position)
      "Change vertical POSITION of OSD object.
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

(define set-osd-align!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_align")
                      (list '* int)))))
    (lambda (osd align)
      "Change horizontal alignment of OSD object.
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

(define set-osd-timeout!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_timeout")
                      (list '* int)))))
    (lambda (osd seconds)
      "Change the number of SECONDS before OSD object is hidden."
      ((force proc) osd seconds))))

(define set-osd-font!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_font")
                      (list '* '*)))))
    (lambda (osd font)
      "Change FONT of OSD object.
FONT is a string with the XLFD full name of the new font (see `xfontsel'
program for details)."
      ((force proc) osd (string->pointer font)))))

(define set-osd-color!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_colour")
                      (list '* '*)))))
    (lambda (osd color)
      "Change COLOR of OSD object.
COLOR is a string with the color value (e.g., `#2e8b57') or the color
name (e.g., `DeepSkyBlue')."
      ((force proc) osd (string->pointer color)))))

(define set-osd-shadow-color!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_shadow_colour")
                      (list '* '*)))))
    (lambda (osd color)
      "Change COLOR of the text shadow of OSD object.
See `set-osd-color!' for the meaning of COLOR."
      ((force proc) osd (string->pointer color)))))

(define set-osd-outline-color!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_outline_colour")
                      (list '* '*)))))
    (lambda (osd color)
      "Change COLOR of the text outline of OSD object.
See `set-osd-color!' for the meaning of COLOR."
      ((force proc) osd (string->pointer color)))))

(define set-osd-vertical-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_vertical_offset")
                      (list '* int)))))
    (lambda (osd offset)
      "Change vertical OFFSET of OSD object.
OFFSET is a number of pixels to offset OSD from its default vertical
position (specified by `set-osd-position!')."
      ((force proc) osd offset))))

(define set-osd-horizontal-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_horizontal_offset")
                      (list '* int)))))
    (lambda (osd offset)
      "Change horizontal OFFSET of OSD object.
OFFSET is a number of pixels to offset OSD from its default
horizontal position (specified by `set-osd-align!')."
      ((force proc) osd offset))))

(define set-osd-shadow-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_shadow_offset")
                      (list '* int)))))
    (lambda (osd offset)
      "Change OFFSET (number of pixels) of the text shadow of OSD object."
      ((force proc) osd offset))))

(define set-osd-outline-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_outline_offset")
                      (list '* int)))))
    (lambda (osd offset)
      "Change OFFSET (number of pixels) of the text outline of OSD object."
      ((force proc) osd offset))))

(define osd-color
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_get_colour")
                      (list '* '* '* '*)))))
    (lambda (osd)
      "Get RGB value of OSD object's color.
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

(define osd-number-of-lines
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_get_number_lines")
                      (list '*)))))
    (lambda (osd)
      "Return the maximum number of lines allowed for OSD object."
      ((force proc) osd))))

(define scroll-osd
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_scroll")
                      (list '* int)))))
    (lambda (osd number-of-lines)
      "Scroll OSD object by NUMBER-OF-LINES."
      ((force proc) osd number-of-lines))))


;;; Higher level procedures

(define* (set-osd! osd #:key align position
                   bar-length timeout color font
                   horizontal-offset vertical-offset
                   outline-offset shadow-offset
                   outline-color shadow-color
                   #:allow-other-keys)
  "Update OSD object with the specified parameters.
See `set-osd-...!' procedures for the meaning of the other arguments."
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
    (set-osd-shadow-color! osd shadow-color)))

(define* (make-osd #:key (lines 1) #:allow-other-keys #:rest args)
  "Create and return a new OSD object with the specified arguments.
LINES is the number of lines that the OSD object can display.
The rest keyword arguments, ARGS, are passed to `set-osd!' procedure."
  (let ((osd (create-osd lines)))
    (apply set-osd! osd args)
    osd))

(define (toggle-osd osd)
  "Hide/show OSD."
  (if (osd-displayed? osd)
      (hide-osd osd)
      (show-osd osd)))

;;; xosd.scm ends here
