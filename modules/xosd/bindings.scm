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
  (let ((lib #f))
    (lambda ()
      "Return the linked 'libxosd' library."
      (or lib
          (begin (set! lib (load-foreign-library "libxosd"))
                 lib)))))

(define (xosd-procedure name)
  "Helper function to get pointers to libxosd procedures."
  (foreign-library-pointer (xosd-library) name))

(define xosd-create
  (let ((proc (xosd-procedure "xosd_create")))
    (pointer->procedure '* proc (list int))))

(define xosd-destroy
  (let ((proc (xosd-procedure "xosd_destroy")))
    (pointer->procedure int proc (list '*))))

(define (xosd-display-string osd line-number string)
  (let ((proc (xosd-procedure "xosd_display")))
    ((pointer->procedure int proc (list '* int int '*))
     osd line-number %XOSD_string (string->pointer string))))

(define (xosd-display-percentage osd line-number percentage)
  (let ((proc (xosd-procedure "xosd_display")))
    ((pointer->procedure int proc (list '* int int int))
     osd line-number %XOSD_percentage percentage)))

(define (xosd-display-slider osd line-number percentage)
  (let ((proc (xosd-procedure "xosd_display")))
    ((pointer->procedure int proc (list '* int int int))
     osd line-number %XOSD_slider percentage)))

(define xosd-hide
  (let ((proc (xosd-procedure "xosd_hide")))
    (pointer->procedure int proc (list '*))))

(define xosd-show
  (let ((proc (xosd-procedure "xosd_show")))
    (pointer->procedure int proc (list '*))))

(define (xosd-onscreen? osd)
  (let ((proc (xosd-procedure "xosd_is_onscreen")))
    ;; TODO 'xosd_is_onscreen' can also return -1 (on failure).
    (integer->bool ((pointer->procedure int proc (list '*))
                    osd))))

(define xosd-wait-until-no-display
  (let ((proc (xosd-procedure "xosd_wait_until_no_display")))
    (pointer->procedure int proc (list '*))))

(define xosd-set-bar-length!
  (let ((proc (xosd-procedure "xosd_set_bar_length")))
    (pointer->procedure int proc (list '* int))))

(define (xosd-set-pos! osd position)
  (let ((proc (xosd-procedure "xosd_set_pos"))
        (pos  (case position
                ((top)    %XOSD_top)
                ((middle) %XOSD_middle)
                ((bottom) %XOSD_bottom)
                (else (begin
                        (print-error "Unknown position: ~a" position)
                        #f)))))
    (when pos
      ((pointer->procedure int proc (list '* int))
       osd pos))))

(define (xosd-set-align! osd position)
  (let ((proc (xosd-procedure "xosd_set_align"))
        (pos  (case position
                ((left)   %XOSD_left)
                ((center) %XOSD_center)
                ((right)  %XOSD_right)
                (else (begin
                        (print-error "Unknown align: ~a" position)
                        #f)))))
    (when pos
      ((pointer->procedure int proc (list '* int))
       osd pos))))

(define xosd-set-timeout!
  (let ((proc (xosd-procedure "xosd_set_timeout")))
    (pointer->procedure int proc (list '* int))))

(define (xosd-set-font! osd font)
  (let ((proc (xosd-procedure "xosd_set_font")))
    ((pointer->procedure int proc (list '* '*))
     osd (string->pointer font))))

(define (xosd-set-colour! osd colour)
  (let ((proc (xosd-procedure "xosd_set_colour")))
    ((pointer->procedure int proc (list '* '*))
     osd (string->pointer colour))))

(define (xosd-set-shadow-colour! osd colour)
  (let ((proc (xosd-procedure "xosd_set_shadow_colour")))
    ((pointer->procedure int proc (list '* '*))
     osd (string->pointer colour))))

(define (xosd-set-outline-colour! osd colour)
  (let ((proc (xosd-procedure "xosd_set_outline_colour")))
    ((pointer->procedure int proc (list '* '*))
     osd (string->pointer colour))))

(define xosd-set-vertical-offset!
  (let ((proc (xosd-procedure "xosd_set_vertical_offset")))
    (pointer->procedure int proc (list '* int))))

(define xosd-set-horizontal-offset!
  (let ((proc (xosd-procedure "xosd_set_horizontal_offset")))
    (pointer->procedure int proc (list '* int))))

(define xosd-set-shadow-offset!
  (let ((proc (xosd-procedure "xosd_set_shadow_offset")))
    (pointer->procedure int proc (list '* int))))

(define xosd-set-outline-offset!
  (let ((proc (xosd-procedure "xosd_set_outline_offset")))
    (pointer->procedure int proc (list '* int))))

(define (xosd-get-colour osd)
  (let* ((endianness (native-endianness))
         (int-size   (sizeof int))
         (red-bv     (make-bytevector int-size))
         (green-bv   (make-bytevector int-size))
         (blue-bv    (make-bytevector int-size))
         (red-ptr    (bytevector->pointer red-bv))
         (green-ptr  (bytevector->pointer green-bv))
         (blue-ptr   (bytevector->pointer blue-bv))
         (proc       (xosd-procedure "xosd_get_colour"))
         ;; Note: 'xosd_get_colour' returns X11 color formats which are
         ;; 16-bit values (0-65535) instead of 8-bit values (0-255).
         (result     ((pointer->procedure int proc (list '* '* '* '*))
                      osd red-ptr green-ptr blue-ptr)))
    (if (= result 0)
        (list (bytevector-sint-ref red-bv 0   endianness int-size)
              (bytevector-sint-ref green-bv 0 endianness int-size)
              (bytevector-sint-ref blue-bv 0  endianness int-size))
        #f)))

(define xosd-get-number-lines
  (let ((proc (xosd-procedure "xosd_get_number_lines")))
    (pointer->procedure int proc (list '*))))

(define xosd-scroll
  (let ((proc (xosd-procedure "xosd_scroll")))
    (pointer->procedure int proc (list '*))))


;;; Documentation

(unless (member %documentation-file-name documentation-files)
  (set! documentation-files (cons %documentation-file-name
                                  documentation-files)))

;;; bindings.scm ends here
