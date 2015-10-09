(define +
  (lambda rest
   (if (null? rest)
    0
    (let loop ((x (car rest)) (rest (cdr rest)))
     (if (null? rest)
      x
      (loop (#%+ x (car rest)) (cdr rest)))))))

(define neg
  (lambda (x)
    (#%- 0 x)))

(define -
  (lambda (x . rest)
    (if (null? rest)
        (neg x)
        (let loop ((x x) (rest rest))
	 (if (null? rest)
	  x
	  (loop (#%- x (car rest)) (cdr rest)))))))

(define *
  (lambda rest
   (if (null? rest)
    1
    (let loop ((x (car rest)) (rest (cdr rest)))
     (if (null? rest)
      x
      (loop (#%mul x (car rest)) (cdr rest)))))))

(define #%mul
  (lambda (x y)
    (let* ((x-neg? (negative? x))
           (y-neg? (negative? y))
           (x      (if x-neg? (neg x) x))
           (y      (if y-neg? (neg y) y)))
      (let ((prod   (#%mul-non-neg x y)))
        (cond ((and x-neg? y-neg?)
               prod)
              ((or x-neg? y-neg?)
               (neg prod))
              (else
               prod))))))

(define quotient ;; TODO similar to #%mul, abstract ?
  (lambda (x y)
    (let* ((x-neg? (negative? x))
           (y-neg? (negative? y))
           (x      (if x-neg? (neg x) x))
           (y      (if y-neg? (neg y) y)))
      (let ((quot   (#%div-non-neg x y)))
        (cond ((and x-neg? y-neg?)
               quot)
              ((or x-neg? y-neg?)
               (neg quot))
              (else
               quot))))))

(define /
  (lambda (x . rest)
    (if (null? rest)
     (quotient 1 x)
     (let loop ((x x) (rest rest))
      (if (null? rest)
       x
       (loop (quotient x (car rest)) (cdr rest)))))))


(define (#%cmp-aux cmp lst)
 (or (null? lst)
     (let loop ((x (car lst)) (lst (cdr lst)))
      (or (null? lst)
          (let ((y (car lst)))
           (and (cmp x y)
                (loop y (cdr lst))))))))

(define =  (lambda rest (#%cmp-aux #%=  rest)))
(define <  (lambda rest (#%cmp-aux #%<  rest)))
(define >  (lambda rest (#%cmp-aux #%>  rest)))
(define <= (lambda rest (#%cmp-aux #%<= rest)))
(define >= (lambda rest (#%cmp-aux #%>= rest)))

(define (#%> a b) (#%< b a))
(define (#%<= a b) (or (#%< a b) (#%= a b)))
(define (#%>= a b) (or (#%> a b) (#%= a b)))

(define (zero?     z) (#%= z 0))
(define (positive? x) (#%> x 0))
(define (negative? x) (#%< x 0))

(define list
  (lambda lst lst))

(define (length lst)
  (let loop ((lst lst) (n 0))
    (if (null? lst)
      n
      (loop (cdr lst) (#%+ n 1)))))

(define append
  (lambda (lst1 lst2)
    (if (pair? lst1)
        (cons (car lst1) (append (cdr lst1) lst2))
        lst2)))

(define reverse
  (lambda (lst)
    (#%reverse-aux lst '())))

(define #%reverse-aux
  (lambda (lst rev)
    (if (pair? lst)
        (#%reverse-aux (cdr lst) (cons (car lst) rev))
        rev)))

(define list-tail
  (lambda (lst i)
    (if (zero? i)
      lst
      (list-tail (cdr lst) (#%- i 1)))))

(define list-ref
  (lambda (lst i)
    (car (list-tail lst i))))

(define list-set!
  (lambda (lst i x)
    (set-car! (list-tail lst i) x)))

(define (#%max a b) (if (#%> a b) a b))

(define max
  (lambda (x1 . rest)
   (let loop ((m x1) (lst rest))
    (if (null? lst)
     m
     (let ((n (car lst)))
      (loop (#%max m n) (cdr lst)))))))

(define (#%min a b) (if (#%< a b) a b))

(define min
  (lambda (x1 . rest)
   (let loop ((m x1) (lst rest))
    (if (null? lst)
     m
     (let ((n (car lst)))
      (loop (#%min m n) (cdr lst)))))))

(define abs
  (lambda (x)
    (if (negative? x) (neg x) x)))

(define remainder #%rem-non-neg)
(define modulo    #%rem-non-neg)

(define #%box (lambda (a) (cons a '())))

(define #%unbox car)

(define #%box-set! set-car!)

(define string
  (lambda chars
    (list->string chars)))

; XXX make-string has limited usefulness without string functions like
; string-ref and string-set!
(define make-string
  (lambda (len . rest)
    (let ((c (if (null? rest) #\space (car rest))))
     (list->string (#%make-string-aux len c '())))))

(define (#%make-string-aux len c lst)
  (if (zero? len)
   lst
   (#%make-string-aux (#%- len 1) c (cons c lst))))

; picobit implements characters as plain integers
(define (char=? c1 c2) (#%= c1 c2))
(define (char<? c1 c2) (#%< c1 c2))

(define (string=? str1 str2)
  (or (eq? str1 str2)
      (#%string=?-aux (string->list str1) (string->list str2))))

(define (#%string=?-aux lst1 lst2)
 (if (null? lst1)
  (null? lst2)
  (and (char=? (car lst1) (car lst2))
       (#%string=?-aux (cdr lst1) (cdr lst2)))))

(define (string<? str1 str2)
  (if (eq? str1 str2)
    #f
    (#%string<?-aux (string->list str1) (string->list str2))))

; I'm sure this could be slimmed down
(define (#%string<?-aux lst1 lst2)
  (cond
   ((null? lst2)
    #f)
   ((null? lst1)
    #t)
   ((char=? (car lst1) (car lst2))
    (#%string<?-aux (cdr lst1) (cdr lst2)))
   (else
    (char<? (car lst1) (car lst2)))))

(define (string>? str1 str2) (string<? str2 str1))
(define (string<=? str1 str2) (or (string<? str1 str2) (string=? str1 str2)))
(define (string>=? str1 str2) (or (string>? str1 str2) (string=? str1 str2)))

(define string-length
  (lambda (str)
    (length (string->list str))))

(define string-append
  (lambda (str . rest)
    (list->string (#%string-append-aux (string->list str) rest))))

(define #%string-append-aux
  (lambda (lst rest)
    (if (null? rest)
        lst
        (#%string-append-aux (append lst (string->list (car rest))) (cdr rest)))))

(define substring
  (lambda (str start end)
    (list->string
     (#%substring-aux2
      (#%substring-aux1 (string->list str) start)
      (#%- end start)))))

(define #%substring-aux1
  (lambda (lst n)
    (if (positive? n)
        (#%substring-aux1 (cdr lst) (#%- n 1))
        lst)))

(define #%substring-aux2
  (lambda (lst n)
    (if (positive? n)
        (cons (car lst) (#%substring-aux2 (cdr lst) (#%- n 1)))
        '())))

(define map
  (lambda (f lst)
    (if (pair? lst)
        (cons (f (car lst))
              (map f (cdr lst)))
        '())))

(define for-each
  (lambda (f lst)
    (if (pair? lst)
        (begin
          (f (car lst))
          (for-each f (cdr lst)))
        #f)))

(define call/cc
  (lambda (receiver)
    (let ((k (get-cont)))
      (receiver
       (lambda (r)
         (return-to-cont k r))))))

(define root-k #f)
(define readyq #f)

(define start-first-process
  (lambda (thunk)
    ;; rest of the program, after call to start-first-process
    (set! root-k (get-cont))
    (set! readyq (cons #f #f))
    ;; initialize thread queue, which is a circular list of continuations
    (set-cdr! readyq readyq)
    (thunk)))

(define spawn
  (lambda (thunk)
    (let* ((k (get-cont))
           (next (cons k (cdr readyq))))
      ;; add a new link to the circular list
      (set-cdr! readyq next)
      ;; Run thunk with root-k as cont.
      (graft-to-cont root-k (lambda () (thunk) (exit))))))

(define exit
  (lambda ()
    (let ((next (cdr readyq)))
      (if (eq? next readyq) ; queue is empty
          #f
          (begin
            ;; step once on the circular list
            (set-cdr! readyq (cdr next))
            ;; invoke next thread
            (return-to-cont (car next) #f))))))

(define yield
  (lambda ()
    (let ((k (get-cont)))
      ;; save the current continuation
      (set-car! readyq k)
      ;; step once on the circular list
      (set! readyq (cdr readyq))
      ;; run the next thread
      (let ((next-k (car readyq)))
        (set-car! readyq #f)
        (return-to-cont next-k #f)))))

(define number->string
  (lambda (n)
    (list->string
     (if (negative? n)
         (cons #\- (#%number->string-aux (neg n) '()))
         (#%number->string-aux n '())))))

(define #%number->string-aux
  (lambda (n lst)
    (let ((rest (cons (#%+ #\0 (remainder n 10)) lst)))
      (if (#%< n 10)
          rest
          (#%number->string-aux (quotient n 10) rest)))))

(define (caar p) (car (car p)))
(define (cadr p) (car (cdr p)))
(define (cdar p) (cdr (car p)))
(define (cddr p) (cdr (cdr p)))
(define (caaar p) (car (car (car p))))
(define (caadr p) (car (car (cdr p))))
(define (cadar p) (car (cdr (car p))))
(define (caddr p) (car (cdr (cdr p))))
(define (cdaar p) (cdr (car (car p))))
(define (cdadr p) (cdr (car (cdr p))))
(define (cddar p) (cdr (cdr (car p))))
(define (cdddr p) (cdr (cdr (cdr p))))
(define (caaaar p) (car (car (car (car p)))))
(define (caaadr p) (car (car (car (cdr p)))))
(define (caadar p) (car (car (cdr (car p)))))
(define (caaddr p) (car (car (cdr (cdr p)))))
(define (cadaar p) (car (cdr (car (car p)))))
(define (cadadr p) (car (cdr (car (cdr p)))))
(define (caddar p) (car (cdr (cdr (car p)))))
(define (cadddr p) (car (cdr (cdr (cdr p)))))
(define (cdaaar p) (cdr (car (car (car p)))))
(define (cdaadr p) (cdr (car (car (cdr p)))))
(define (cdadar p) (cdr (car (cdr (car p)))))
(define (cdaddr p) (cdr (car (cdr (cdr p)))))
(define (cddaar p) (cdr (cdr (car (car p)))))
(define (cddadr p) (cdr (cdr (car (cdr p)))))
(define (cdddar p) (cdr (cdr (cdr (car p)))))
(define (cddddr p) (cdr (cdr (cdr (cdr p)))))

(define equal?
  (lambda (x y)
    (cond ((eq? x y)
	   #t)
	  ((and (string? x) (string? y))
	   (string=? x y))
	  ((and (pair? x) (pair? y))
	   (and (equal? (car x) (car y))
		(equal? (cdr x) (cdr y))))
	  ((and (u8vector? x) (u8vector? y))
	   (u8vector-equal? x y))
	  (else
	   #f))))

(define u8vector-equal?
  (lambda (x y)
    (let ((lx (u8vector-length x)))
      (if (#%= lx (u8vector-length y))
	  (#%u8vector-equal?-loop x y (#%- lx 1))
	  #f))))
(define #%u8vector-equal?-loop
  (lambda (x y l)
    (if (zero? l)
	#t
	(and (#%= (u8vector-ref x l) (u8vector-ref y l))
	     (#%u8vector-equal?-loop x y (#%- l 1))))))

#|
(define assoc
  (lambda (t l)
    (cond ((null? l)
	   #f)
	  ((equal? t (caar l))
	   (car l))
	  (else
	   (assoc t (cdr l))))))

(define memq
  (lambda (t l)
    (cond ((null? l)
	   #f)
	  ((eq? (car l) t)
	   l)
	  (else
	   (memq t (cdr l))))))
|#

(define (#%mem-aux obj lst f? accessor)
  (cond ((null? lst) #f)
        ((f? (accessor (car lst)) obj) lst)
	(else (#%mem-aux obj (cdr lst) f? accessor))))
(define (#%ass-aux obj alst f?)
 (let ((x (#%mem-aux obj alst f? car)))
  (and x (car x))))

(define (memq   obj lst) (#%mem-aux obj lst eq?    values))
(define (memv   obj lst) (#%mem-aux obj lst eqv?   values))
(define (member obj lst) (#%mem-aux obj lst equal? values))
(define (assq   obj alst) (#%ass-aux obj alst eq?))
(define (assv   obj alst) (#%ass-aux obj alst eqv?))
(define (assoc  obj alst) (#%ass-aux obj alst equal?))

;; XXX this is not 100% correct
(define values
 (lambda rest
  (if (null? rest)
   #f ; XXX
   (car rest))))

(define eqv? equal?) ; TODO implement a real eqv? function

(define vector list)
(define vector-ref list-ref)
(define vector-set! list-set!)

(define u8vector
  (lambda x
    (list->u8vector x)))
(define list->u8vector
  (lambda (x)
    (let* ((n (length x))
	   (v (#%make-u8vector n)))
      (#%list->u8vector-loop v 0 x)
      v)))
(define #%list->u8vector-loop
  (lambda (v n x)
    (u8vector-set! v n (car x))
    (if (not (null? (cdr x)))
	(#%list->u8vector-loop v (#%+ n 1) (cdr x)))))
(define make-u8vector
  (lambda (n x)
    (#%make-u8vector-loop (#%make-u8vector n) (#%- n 1) x)))
(define #%make-u8vector-loop
  (lambda (v n x)
    (if (negative? n)
        v
        (begin (u8vector-set! v n x)
               (#%make-u8vector-loop v (#%- n 1) x)))))
(define u8vector-copy!
  (lambda (source source-start target target-start n)
    (if (positive? n)
        (begin (u8vector-set! target target-start
                              (u8vector-ref source source-start))
               (u8vector-copy! source (#%+ source-start 1)
                               target (#%+ target-start 1)
                               (#%- n 1))))))
