(define light adc)

(define (putchar c)
    (#%putchar c 3))

(define (getchar)
    (or (#%getchar-wait 0 3)
        (getchar)))

(define (getchar-wait duration)
    (#%getchar-wait duration 3))

(define (sleep duration)
    (#%sleep-aux (#%+ (clock) duration)))

(define (#%sleep-aux wake-up)
    (if (< (clock) wake-up)
        (#%sleep-aux wake-up)
        #f))


(define (led2-color state)
    (if (eq? state 'red)
        (#%led2-color 1)
        (#%led2-color 0)))

(define (display x)
    (if (string? x)
        (for-each putchar (string->list x))
        (write x)))

(define (newline) (#%putchar #\newline 3))

(define (displayln x) (display x) (newline))

(define (write x)
    (cond ((string? x)
	   (begin (#%putchar #\" 3)
		  (display x)
		  (#%putchar #\" 3)))
	  ((number? x)
	   (display (number->string x)))
	  ((pair? x)
	   (begin (#%putchar #\( 3)
                  (write (car x))
                  (#%write-list (cdr x))))
	  ((symbol? x)
	   (display "#<symbol>"))
	  ((boolean? x)
	   (display (if x "#t" "#f")))
	  (else
	   (display "#<object>"))))
;; TODO have vectors and co ?

(define (#%write-list lst)
    (cond ((null? lst)
	   (#%putchar #\) 3))
	  ((pair? lst)
	   (begin (#%putchar #\space 3)
		  (write (car lst))
		  (#%write-list (cdr lst))))
	  (else
	   (begin (display " . ")
		  (write lst)
		  (#%putchar #\) 3)))))

(define (pp x)
    (write x)
    (#%putchar #\newline 3))

(define current-time clock)
(define (time->seconds t) (quotient t 100))
