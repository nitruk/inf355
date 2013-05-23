#lang racket

(require (for-syntax scheme))

(define test
  (lambda (a b)
    (if (equal? a b)
        (void)
        (error "Bad test result"))))

(define next-odd
  (lambda (n)
    (+ (* (floor (/ (+ n 1) 2)) 2) 1)))

(define prime?
  (lambda (n)
    (cond
      ((<= n 1) #f)
      ((equal? n 2) #t)
      (else
       (let ((b #f))
         (begin
           (for-each (lambda (i) (set! b (or b (equal? (modulo n i) 0))))
                     (range 2 (+ (sqrt n) 1)))
           (not b)))))))

(define map-interval
  (lambda (f min max)
    (map f (range min (+ max 1)))))

(define iota range)

(test (next-odd 1) 3)
(test (next-odd 2) 3)
(test (prime? -5) #f)
(test (prime? 0) #f)
(test (prime? 1) #f)
(test (prime? 2) #t)
(test (prime? 3) #t)
(test (prime? 19) #t)
(test (prime? 21) #f)
(test (map-interval (lambda (x) (+ 2 x)) 10 13) '(12 13 14 15))
(test (iota 5) '(0 1 2 3 4))
(test (iota 0) '())
(test (iota -1) '())



(define revsymb
  (lambda (x)
    (string->symbol (list->string (reverse (string->list(symbol->string x)))))))

(define trans
  (lambda (x)
    (map revsymb x)))

(define display-all
  (lambda  (x)
    (for-each
     (lambda (y)
       (begin
         (display y)
         (newline)))
     x)))

(define filter
  (lambda (test l)
    (cond
      ((empty? l) '())
      (else (if (test (car l))
                (cons (car l) (filter test (cdr l)))
                (filter test (cdr l)))))))

(define slash
  (lambda (op l)
    (if (empty? l) (error "slash cannot take an empty list")
        (let ((a (car l)) (queue (cdr l)))
          (if (empty? queue)
              a
              (let ((b (car queue)))
                (slash op (cons (op a b) (cdr queue)))))))))

(test (revsymb 'foobar) 'raboof)
(test (trans '(foo bar)) '(oof rab))
(test (filter (lambda (x) (> x 3)) '(1 10 2 20)) '(10 20))
(test (slash * '(10 20 30)) 6000)
(test (slash string-append '("foo" "bar")) "foobar")
(test (slash + '(1 2 3)) 6)
(test (slash - '(10 2 3)) 5)
(test (slash expt '(2 3 4)) 4096)
(test (slash * (filter prime? (iota 100))) 2305567963945518424753102147331756070)


(require (lib "defmacro.ss"))

(define-syntax myand
  (syntax-rules ()
    ((myand)
     #t)
    ((myand a)
     a)
    ((myand a b ...)
     (if a
         (myand b ...)
         #f))))

(test (myand 12 3 #f 5) #f)
(test (myand 12 3 7 5) 5)

(define-syntax myor
  (syntax-rules ()
    ((myor)
     #f)
    ((myor a b ...)
     (let ((tmp a))
       (if tmp
           tmp
           (myor b ...))))))

(test (myor #f #f #f #f) #f)
(test (myor #f #f 42 #f) 42)
(test
 (let ((x 1))
   (+ (myor (begin (set! x (+ 1 x)) x)) x) 4)
 4)

(define-syntax while
  (syntax-rules ()
    ((while c body ...)
     (let loop()
       (if c
           (begin
             body ...
             (loop))
           (void))))))

(test (let ((i 0) (c 0)) (while (< i 5) (set! c (+ c i)) (set! i (+ i 1))) c) 10)

(define-syntax define-trace
  (syntax-rules ()
    ((define-trace object (lambda args body))
     (define object (lambda args
       (begin
         (print (string-append "Entering " (symbol->string 'f)))
         (newline)
         (begin0
           body
           (newline)
           (print (string-append "Exiting " (symbol->string 'f)))
           (newline))))))
    ((define-trace object body)
     (define object
       (begin
         (print (string-append "Entering " (symbol->string 'f)))
         (newline)
         (begin0
           body
           (newline)
           (print (string-append "Exiting " (symbol->string 'f)))
           (newline)))))
    ))

(define-trace foo (lambda (bar) (+ bar 5)))
(define-trace (too bar) (+ bar 5))

(define-syntax (contract x)
    (syntax-case x (pre post inv)
      ((_ expr ...)
       (let ((pre (datum->syntax x 'pre))
         (post (datum->syntax x 'post))
         (inv (datum->syntax x 'inv)))
         #`(begin
             (let ((postlist null)
                 (#,pre (lambda (y) (if y (void) (error "Condition broken before block execution")))))
              (let-syntax
                  ((#,post (syntax-rules ()
                                   ((#,post y) (set! postlist (cons (delay y) postlist)))))
                   (#,inv (syntax-rules ()
                            ((#,inv y) (begin (if y (void) (error "Condition broken before block execution")) (set! postlist (cons (delay y) postlist)))))))
                (begin
                  expr ...
                 (for-each (lambda (y) (if (force y) (void) (error "Condition broken at end of block"))) postlist)))))))))

(let ((i 1))
  (contract (pre (< i 2)) (post (< i 10)) (inv (> i 0)) (while (< i 9) (begin (set! i (+ 1 i))))))

(define-syntax (define-data x)
    (syntax-case x ()
      ((_ name field1 ...)
       (let ((constructor (datum->syntax x (string->symbol (string-append "<" (symbol->string (syntax->datum #'name)) ">")))))
         (begin0
           #`(define #,constructor
               (lambda ()
                 (let ((field1 null) ...)
                   (lambda (op . val)
                     (case op
                       ((string->symbol (string-append "get-" (symbol->string (syntax->datum field1)))) field1) ...
                       ((string->symbol (string-append "set-" (symbol->string (syntax->datum field1)))) (set! field1 val)) ...
                       (else (list (string->symbol (string-append "get-" (symbol->string (syntax->datum #'field1)))) ...))
                       )))))
           (let-syntax ((define-field
                          (lambda x (syntax-case x ()
                                      ((_) #'(void))
                                      ((_ f1 f2 ...)
                                       (let
                                           ((get (datum->syntax x (string->symbol (string-append (symbol->string (syntax->datum #'f1)) ">>"))))
                                            (set (datum->syntax x (string->symbol (string-append ">>" (symbol->string (syntax->datum #'f1)))))))
                                         #`(define-values (#,get #,set)
                                             ((lambda (obj) (obj (string->symbol (string-append "get-" (symbol->string (syntax->datum #'field1))))))
                                              (lambda (obj val) (obj (string->symbol (string-append "set-" (symbol->string (syntax->datum #'field1)))) val))) 
                                             )))))))
             #'(define-field field1 ...)))))))

(define-data student grade age class)
(define student0 (<student>))
(student0 'get-age)
