#lang racket

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

(define aerate
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

(define-syntax while
  (syntax-rules ()
    ((while c body)
     (if (c)
         (begin
           (body)
           (while c body))
         void))))

(let* ((i 0) (f (lambda () (< i 5)))  (g (lambda () (begin (display i) (set! i (+ i 1))))))
  (while f g))