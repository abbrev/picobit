(define +
  (lambda (x . rest)
    (if (pair? rest)
        (#%+-aux x rest)
        x)))

(define (#%+-aux x rest)
    (if (pair? rest)
        (#%+-aux (#%+ x (car rest)) (cdr rest))
        x))

(define (neg x)
    (- 0 x))

(define -
  (lambda (x . rest)
    (if (pair? rest)
        (#%--aux x rest)
        (neg x))))

(define (#%--aux x rest)
    (if (pair? rest)
        (#%--aux (#%- x (car rest)) (cdr rest))
        x))

(define *
  (lambda (x . rest)
    (if (pair? rest)
        (#%*-aux x rest)
        x)))

(define (#%*-aux x rest)
    (if (pair? rest)
        (#%*-aux (#%mul x (car rest)) (cdr rest))
        x))

(define (#%mul x y)
    (let* ((x-neg? (< x 0))
           (y-neg? (< y 0))
           (x      (if x-neg? (neg x) x))
           (y      (if y-neg? (neg y) y)))
      (let ((prod   (#%mul-non-neg x y)))
        (cond ((and x-neg? y-neg?)
               prod)
              ((or x-neg? y-neg?)
               (neg prod))
              (else
               prod)))))

(define (quotient x y) ;; TODO similar to #%mul, abstract ?
    (let* ((x-neg? (< x 0))
           (y-neg? (< y 0))
           (x      (if x-neg? (neg x) x))
           (y      (if y-neg? (neg y) y)))
      (let ((quot   (#%div-non-neg x y)))
        (cond ((and x-neg? y-neg?)
               quot)
              ((or x-neg? y-neg?)
               (neg quot))
              (else
               quot)))))

(define / quotient)


(define (<= x y)
    (or (< x y) (= x y)))

(define (>= x y)
    (or (> x y) (= x y)))

(define list
  (lambda lst lst))

(define (length lst)
    (#%length-aux lst 0))

(define (#%length-aux lst n)
    (if (pair? lst)
        (#%length-aux (cdr lst) (#%+ n 1))
        n))

(define (append lst1 lst2)
    (if (pair? lst1)
        (cons (car lst1) (append (cdr lst1) lst2))
        lst2))

(define (reverse lst)
    (#%reverse-aux lst '()))

(define (#%reverse-aux lst rev)
    (if (pair? lst)
        (#%reverse-aux (cdr lst) (cons (car lst) rev))
        rev))

(define (list-ref lst i)
    (if (= i 0)
        (car lst)
        (list-ref (cdr lst) (#%- i 1))))

(define (list-set! lst i x)
    (if (= i 0)
        (set-car! lst x)
        (list-set! (cdr lst) (#%- i 1) x)))

(define (max x y)
    (if (> x y) x y))

(define (min x y)
    (if (< x y) x y))

(define (abs x)
    (if (< x 0) (neg x) x))

(define remainder #%rem-non-neg)
(define modulo    #%rem-non-neg)

(define (#%box a) (cons a '()))

(define #%unbox car)

(define #%box-set! set-car!)

(define string
  (lambda chars
    (list->string chars)))

(define (string-length str)
    (length (string->list str)))

(define (string-append str1 str2)
    (list->string (append (string->list str1) (string->list str2))))

(define (substring str start end)
    (list->string
     (#%substring-aux2
      (#%substring-aux1 (string->list str) start)
      (#%- end start))))

(define (#%substring-aux1 lst n)
    (if (>= n 1)
        (#%substring-aux1 (cdr lst) (#%- n 1))
        lst))

(define (#%substring-aux2 lst n)
    (if (>= n 1)
        (cons (car lst) (#%substring-aux2 (cdr lst) (#%- n 1)))
        '()))

(define (map f lst)
    (if (pair? lst)
        (cons (f (car lst))
              (map f (cdr lst)))
        '()))

(define (for-each f lst)
    (if (pair? lst)
        (begin
          (f (car lst))
          (for-each f (cdr lst)))
        #f))

(define (call/cc receiver)
    (let ((k (get-cont)))
      (receiver
       (lambda (r)
         (return-to-cont k r)))))

(define root-k #f)
(define readyq #f)

(define (start-first-process thunk)
    ;; rest of the program, after call to start-first-process
    (set! root-k (get-cont))
    (set! readyq (cons #f #f))
    ;; initialize thread queue, which is a circular list of continuations
    (set-cdr! readyq readyq)
    (thunk))

(define (spawn thunk)
    (let* ((k (get-cont))
           (next (cons k (cdr readyq))))
      ;; add a new link to the circular list
      (set-cdr! readyq next)
      ;; Run thunk with root-k as cont.
      (graft-to-cont root-k (lambda () (thunk) (exit)))))

(define (exit)
    (let ((next (cdr readyq)))
      (if (eq? next readyq) ; queue is empty
          #f
          (begin
            ;; step once on the circular list
            (set-cdr! readyq (cdr next))
            ;; invoke next thread
            (return-to-cont (car next) #f)))))

(define (yield)
    (let ((k (get-cont)))
      ;; save the current continuation
      (set-car! readyq k)
      ;; step once on the circular list
      (set! readyq (cdr readyq))
      ;; run the next thread
      (let ((next-k (car readyq)))
        (set-car! readyq #f)
        (return-to-cont next-k #f))))

(define (number->string n)
    (list->string
     (if (< n 0)
         (cons #\- (#%number->string-aux (neg n) '()))
         (#%number->string-aux n '()))))

(define (#%number->string-aux n lst)
    (let ((rest (cons (#%+ #\0 (remainder n 10)) lst)))
      (if (< n 10)
          rest
          (#%number->string-aux (quotient n 10) rest))))

(define (caar p)
    (car (car p)))
(define (cadr p)
    (car (cdr p)))
(define (cdar p)
    (cdr (car p)))
(define (cddr p)
    (cdr (cdr p)))
(define (caaar p)
    (car (car (car p))))
(define (caadr p)
    (car (car (cdr p))))
(define (cadar p)
    (car (cdr (car p))))
(define (caddr p)
    (car (cdr (cdr p))))
(define (cdaar p)
    (cdr (car (car p))))
(define (cdadr p)
    (cdr (car (cdr p))))
(define (cddar p)
    (cdr (cdr (car p))))
(define (cdddr p)
    (cdr (cdr (cdr p))))

(define (equal? x y)
    (cond ((eq? x y)
	   #t)
	  ((and (pair? x) (pair? y))
	   (and (equal? (car x) (car y))
		(equal? (cdr x) (cdr y))))
	  ((and (u8vector? x) (u8vector? y))
	   (u8vector-equal? x y))
	  (else
	   #f)))

(define (u8vector-equal? x y)
    (let ((lx (u8vector-length x)))
      (if (= lx (u8vector-length y))
	  (#%u8vector-equal?-loop x y (- lx 1))
	  #f)))
(define (#%u8vector-equal?-loop x y l)
    (if (= l 0)
	#t
	(and (= (u8vector-ref x l) (u8vector-ref y l))
	     (#%u8vector-equal?-loop x y (#%- l 1)))))

(define (assoc t l)
    (cond ((null? l)
	   #f)
	  ((equal? t (caar l))
	   (car l))
	  (else
	   (assoc t (cdr l)))))

(define (memq t l)
    (cond ((null? l)
	   #f)
	  ((eq? (car l) t)
	   l)
	  (else
	   (memq t (cdr l)))))

(define vector list)
(define vector-ref list-ref)
(define vector-set! list-set!)

(define u8vector
  (lambda x
    (list->u8vector x)))
(define (list->u8vector x)
    (let* ((n (length x))
	   (v (#%make-u8vector n)))
      (#%list->u8vector-loop v 0 x)
      v))
(define (#%list->u8vector-loop v n x)
    (u8vector-set! v n (car x))
    (if (not (null? (cdr x)))
	(#%list->u8vector-loop v (#%+ n 1) (cdr x))))
(define (make-u8vector n x)
    (#%make-u8vector-loop (#%make-u8vector n) (- n 1) x))
(define (#%make-u8vector-loop v n x)
    (if (>= n 0)
        (begin (u8vector-set! v n x)
               (#%make-u8vector-loop v (- n 1) x))
        v))
(define (u8vector-copy! source source-start target target-start n)
    (if (> n 0)
        (begin (u8vector-set! target target-start
                              (u8vector-ref source source-start))
               (u8vector-copy! source (+ source-start 1)
                               target (+ target-start 1)
                               (- n 1)))))
