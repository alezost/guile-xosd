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
  #:use-module (ice-9 documentation)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (xosd config)
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
      ((force proc) number-of-lines))))

(define xosd-destroy
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_destroy")
                      (list '*)))))
    (lambda (osd)
      ((force proc) osd))))

(define xosd-display-string
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_display")
                      (list '* int int '*)))))
    (lambda (osd line-number string)
      ((force proc) osd line-number %XOSD_string (string->pointer string)))))

(define-values (xosd-display-percentage
                xosd-display-slider)
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_display")
                      (list '* int int int)))))
    (values
     (lambda (osd line-number percentage)
       ((force proc) osd line-number %XOSD_percentage percentage))
     (lambda (osd line-number percentage)
       ((force proc) osd line-number %XOSD_slider percentage)))))

(define xosd-hide
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_hide")
                      (list '*)))))
    (lambda (osd)
      ((force proc) osd))))

(define xosd-show
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_show")
                      (list '*)))))
    (lambda (osd)
      ((force proc) osd))))

(define xosd-onscreen?
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_is_onscreen")
                      (list '*)))))
    (lambda (osd)
      ;; TODO 'xosd_is_onscreen' can also return -1 (on failure).
      (integer->bool ((force proc) osd)))))

(define xosd-wait-until-no-display
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_wait_until_no_display")
                      (list '*)))))
    (lambda (osd)
      ((force proc) osd))))

(define xosd-set-bar-length!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_bar_length")
                      (list '* int)))))
    (lambda (osd length)
      ((force proc) osd length))))

(define xosd-set-pos!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_pos")
                      (list '* int)))))
    (lambda (osd position)
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
      ((force proc) osd time))))

(define xosd-set-font!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_font")
                      (list '* '*)))))
    (lambda (osd font)
      ((force proc) osd (string->pointer font)))))

(define xosd-set-colour!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_colour")
                      (list '* '*)))))
    (lambda (osd color)
      ((force proc) osd (string->pointer color)))))

(define xosd-set-shadow-colour!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_shadow_colour")
                      (list '* '*)))))
    (lambda (osd color)
      ((force proc) osd (string->pointer color)))))

(define xosd-set-outline-colour!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_outline_colour")
                      (list '* '*)))))
    (lambda (osd color)
      ((force proc) osd (string->pointer color)))))

(define xosd-set-vertical-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_vertical_offset")
                      (list '* int)))))
    (lambda (osd offset)
      ((force proc) osd offset))))

(define xosd-set-horizontal-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_horizontal_offset")
                      (list '* int)))))
    (lambda (osd offset)
      ((force proc) osd offset))))

(define xosd-set-shadow-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_shadow_offset")
                      (list '* int)))))
    (lambda (osd offset)
      ((force proc) osd offset))))

(define xosd-set-outline-offset!
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_set_outline_offset")
                      (list '* int)))))
    (lambda (osd offset)
      ((force proc) osd offset))))

(define xosd-get-colour
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_get_colour")
                      (list '* '* '* '*)))))
    (lambda (osd)
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
      ((force proc) osd))))

(define xosd-scroll
  (let ((proc (delay (pointer->procedure
                      int (xosd-procedure "xosd_scroll")
                      (list '* int)))))
    (lambda (osd number-of-lines)
      ((force proc) osd number-of-lines))))


;;; Documentation

(unless (member %documentation-file-name documentation-files)
  (set! documentation-files (cons %documentation-file-name
                                  documentation-files)))

;;; bindings.scm ends here
