(define *global-a* (cons 1 2))
(define *global-b* *global-a*)

;;;; There is a bug in the Picobit compiler: if the modify-global-b! function
;;;; is defined, the compiler does not generate any assembly code to set
;;;; *global-a* (it remains set to #f), and *global-b* is set to *global-a*.
;;;; The cons is performed, but its return value is simply discarded.
(define (modify-global-b!) (set! *global-b* 1))

;;;; The bug is not triggered if there is an explicit reference to *global-a*.
;*global-a*

;;;; The bug is not triggered if *global-a* is set to some other value, such as
;;;; a number or #t or a list or any constant value.

;;;; Expected output: (1 . 2)
;;;; Actual output: #f
(displayln *global-b*)
