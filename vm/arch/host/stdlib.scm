(define light adc)

; an I/O port consists of a file descriptor and a peek char
(define (#%make-port fd) (cons fd #f))
(define (#%port-get-fd port)
 (if (pair? port)
  (car port)
  (begin
   (#%write-char #\E 2)
   (#%write-char #\f 2)
   2)))
(define (#%port-get-peek-char port)
 (if (pair? port)
  (cdr port)
  (begin
   (#%write-char #\E 2)
   (#%write-char #\g 2)
   #f)))
(define (#%port-set-peek-char! port c)
 (if (pair? port)
  (set-cdr! port c)
  (begin
   (#%write-char #\E 2)
   (#%write-char #\g 2)
  )))

(define *stdin*  (#%make-port 0))
(define *stdout* (#%make-port 1))
(define *stderr* (#%make-port 2))

; I/O ports:
; 0 = stdin
; 1 = stdout
; 2 = stderr
(define #%*current-input-port* *stdin*)
(define #%*current-output-port* *stdout*)

(define (current-input-port)
 (#%write-char #\I 1)
 (display "(current-input-port) => " *stderr*)
 (displayln #%*current-input-port* *stderr*)
 (display "*stdin* => " *stderr*)
 (displayln *stdin* *stderr*)
 #%*current-input-port*)
(define (current-output-port)
 (#%write-char #\O 2)
 ;(display "(current-output-port) => " *stderr*)
 ;(displayln #%*current-output-port* *stderr*)
 ;(display "*stdout* is a port => " *stderr*)
 ;(displayln (pair? *stdout*) *stderr*)
 #%*current-output-port*)

;;;; the following two functions are not standard but mimic
;;;; with-input-from-file and with-output-to-file

(define (with-input-from-port port proc)
 (let ((old #%*current-input-port*))
  (set! #%*current-input-port* port)
  (let ((n (proc)))
   (set! #%*current-input-port* old)
   n)))

(define (with-output-to-port port proc)
 (let ((old #%*current-output-port*))
  (set! #%*current-output-port* port)
  (let ((n (proc)))
   (set! #%*current-output-port* old)
   n)))


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
   (#%write-char c (#%port-get-fd (#%get-output-port rest)))))

; '#%eof does not eq? any other object
(define (eof-object? obj) (eq? obj '#%eof))

(define char-ready?
  (lambda rest
   (#%char-ready? (#%get-input-port rest))))

; if a peek char exists for port, return it; if keep is false, delete the peek
; char before returning
; if a peek char does not exist for port, read a char and return it; if keep is
; true, save the peek char before returning
(define (#%peek-or-read-char port keep)
  (let ((pc (#%port-get-peek-char port)))
   (if pc
     (let ((c pc))
       (if (not keep) (#%port-set-peek-char! port #f))
       c)
     (let* ((c (or (#%read-char (#%port-get-fd port)) '#%eof)))
      (if keep (#%port-set-peek-char! port c))
      c))))

(define peek-char
  (lambda rest
   (#%peek-or-read-char (#%get-input-port rest) #t)))

(define read-char
  (lambda rest
   (#%peek-or-read-char (#%get-input-port rest) #f)))

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
  (let ((fd (#%port-get-fd port))) (for-each (lambda (c) (#%write-char c fd)) (string->list x)))
  (#%write x port)))

(define display
  (lambda (x . rest)
   (#%display x (#%get-output-port rest))))

(define (#%newline port)
  (#%write-char #\newline (#%port-get-fd port)))

(define newline
  (lambda rest
   (#%newline (#%get-output-port rest))))

(define displayln
  (lambda (x . rest)
   (let ((port (#%get-output-port rest)))
    (#%display x port) (#%newline port))))

(define (#%write x port)
    (cond ((string? x)
	   (begin (#%write-char #\" (#%port-get-fd port))
		  (#%display x port)
		  (#%write-char #\" (#%port-get-fd port))))
	  ((number? x)
	   (#%display (number->string x) port))
	  ((pair? x)
	   (begin (#%write-char #\( (#%port-get-fd port))
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
	   (#%write-char #\) (#%port-get-fd port)))
	  ((pair? lst)
	   (begin (#%write-char #\space (#%port-get-fd port))
		  (#%write (car lst) port)
		  (#%write-list (cdr lst) port)))
	  (else
	   (begin (#%display " . " port)
		  (#%write lst port)
		  (#%write-char #\) (#%port-get-fd port)))))))

(define pp
  (lambda (x . rest)
   (let ((port (#%get-output-port rest)))
    (#%write x port)
    (#%write-char #\newline (#%port-get-fd port)))))

(define current-time clock)
(define time->seconds (lambda (t) (quotient t 100)))
