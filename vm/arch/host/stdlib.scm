(define light adc)

; I/O ports:
; 0 = stdin
; 1 = stdout
; 2 = stderr
; XXX ports are not implemented in the VM yet
(define #%*current-input-port* 3)
(define #%*current-output-port* 3)

(define (current-input-port) #%*current-input-port*)
(define (current-output-port) #%*current-output-port*)

(define (#%get-input-port rest)
  (if (null? rest)
   (current-input-port)
   (car rest)))

(define (#%get-output-port rest)
  (if (null? rest)
   (current-output-port)
   (car rest)))

(define write-char
  (lambda (c . rest)
   (#%write-char c (#%get-output-port rest))))

; '#%eof does not eq? any other object
(define (eof-object? obj) (eq? obj '#%eof))

(define #%*peek-char* #f) ; TODO this should be a map of port => peek-char

(define (#%set-peek-char! c port)
  (set! #%*peek-char* c))

(define (#%get-peek-char port)
  #%*peek-char*)

(define char-ready?
  (lambda rest
   (#%char-ready? (#%get-input-port rest))))

(define peek-char
  (lambda rest
   (let* ((port (#%get-input-port rest)) (pc (#%get-peek-char port)))
    (or pc
     (let ((c (or (#%read-char port) '#%eof)))
      (#%set-peek-char! c port)
      c)))))

(define read-char
  (lambda rest
   (let ((port (#%get-input-port rest)))
    (let ((pc (peek-char port)))
     (if pc (#%set-peek-char! #f port))
     pc))))

(define sleep
  (lambda (duration)
    (#%sleep-aux (#%+ (clock) duration))))

(define #%sleep-aux
  (lambda (wake-up)
    (if (< (clock) wake-up)
        (#%sleep-aux wake-up)
        #f)))


(define led2-color
  (lambda (state)
    (if (eq? state 'red)
        (#%led2-color 1)
        (#%led2-color 0))))

(define (#%display x port)
 (if (string? x)
  (for-each (lambda (c) (#%write-char c port)) (string->list x))
  (#%write x port)))

(define display
  (lambda (x . rest)
   (#%display x (#%get-output-port rest))))

(define (#%newline port)
  (#%write-char #\newline port))

(define newline
  (lambda rest
   (#%newline (#%get-output-port rest))))

(define displayln
  (lambda (x . rest)
   (let ((port (#%get-output-port rest)))
    (#%display x port) (#%newline port))))

(define (#%write x port)
    (cond ((string? x)
	   (begin (#%write-char #\" port)
		  (#%display x port)
		  (#%write-char #\" port)))
	  ((number? x)
	   (#%display (number->string x) port))
	  ((pair? x)
	   (begin (#%write-char #\( port)
                  (#%write (car x) port)
                  (#%write-list (cdr x) port)))
	  ((symbol? x)
	   (#%display "#<symbol>" port))
	  ((boolean? x)
	   (#%display (if x "#t" "#f") port))
	  (else
	   (#%display "#<object>" port))))
;; TODO have vectors and co ?

(define write
  (lambda (x . rest)
   (#%write x (#%get-output-port rest))))

(define #%write-list
  (lambda (lst port)
    (cond ((null? lst)
	   (#%write-char #\) port))
	  ((pair? lst)
	   (begin (#%write-char #\space port)
		  (#%write (car lst) port)
		  (#%write-list (cdr lst) port)))
	  (else
	   (begin (#%display " . " port)
		  (#%write lst port)
		  (#%write-char #\) port))))))

(define pp
  (lambda (x . rest)
   (let ((port (#%get-output-port rest)))
    (#%write x port)
    (#%write-char #\newline port))))

(define current-time clock)
(define time->seconds (lambda (t) (quotient t 100)))
