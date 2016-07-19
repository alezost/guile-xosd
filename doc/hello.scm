(use-modules (xosd))

(let ((osd (make-osd #:timeout 3
                     #:position 'top
                     #:align 'right
                     #:font "-*-dejavu sans-bold-r-normal-*-*-600-*-*-p-*-*-1"
                     #:color "yellow"
                     #:shadow-color "red"
                     #:shadow-offset 3)))
  (display-string-in-osd osd "Hello!")
  (wait-while-osd-displayed osd)
  (kill-osd osd))
