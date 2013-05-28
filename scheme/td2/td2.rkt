#lang racket

(define (fail) (error "No more alternatives"))

(define-syntax amb
  (syntax-rules ()
    ((_) (fail))
    ((_ exp1 exp2 ...) (let/cc k
                       (let ((old-fail fail))
                         (set! fail (lambda ()
                                      (set! fail old-fail)
                                      (k (amb exp2 ...))))
                         exp1)))))

(define (mult)
  (let ((x (amb 1 2 3 4 5 6 7 8 9 10))
        (y (amb 1 2 3 4 5 6 7 8 9 10)))
     (if (= (* x y) 30) (list x y) (amb))))
(mult)

(define (bag-of f)
  (let ((l null))
    (if (amb #t #f)
        (begin (set! l (cons (f) l))
               (fail))
        (reverse l))))
(bag-of mult)

(define (check column row placed)
  (for-each (lambda (x)
              (let ((c (car x))
                    (r (cdr x)))
                (if (or (eq? c column) (eq? r row) (eq? (- r c) (- row column)) (eq? (+ r c) (+ row column)))
                    (amb)
                    (void))))
            placed))

(define (queens)
  (letrec ((loop (lambda (placed)
                   (let ((column (+ (length placed) 1))
                         (row (amb 1 2 3 4 5 6 7 8)))
                     (begin
                       (check column row placed)
                       (let ((new-placed (cons (cons column row) placed)))
                             (if (< column 8)
                                 (loop new-placed)
                                 new-placed)))))))
    (reverse (loop null))))

(length (bag-of queens))

(define-syntax while
  (syntax-rules ()
    ((while c body ...)
     (let loop()
       (if c
           (begin
             body ...
             (loop))
           (void))))))

(define-values (start-thread scheduler yield)
  (let* ((threads null)
         (schedule #f)
         (scheduler (lambda ()
                      (while (not (null? threads))
                             (let ((thread (car threads)))
                               (begin
                                 (set! threads (cdr threads))
                                 (let ((thr (let/cc k
                                              (begin0
                                                (set! schedule k)
                                                (call/cc thread)))))
                                       (if thr
                                           (set! threads (reverse (cons thr (reverse threads))))
                                           (void))))))))
         (yield (lambda ()
                       (call/cc schedule)))
         (start-thread (lambda x
                         (let ((f (car x)) (args (cdr x)))
                           (if (let/cc k
                                 (set! threads (cons k threads))
                                 #false)
                             (begin
                               (apply f args)
                               (schedule #f))
                             (void))))))
    (values start-thread scheduler yield)))

(define (displaynl m)
  (display m)
  (newline))

(define (thread n)
  (displaynl (list "Starting thread" n))
  (yield)
  (displaynl (list "In thread" n))
  (yield)
  (displaynl (list "Ending thread" n)))

(define (thread2)
  (displaynl "Starting thread2")
  (yield)
  (displaynl "In thread2")
  (yield)
  (displaynl "Creating thread 4 while scheduler is already running")
  (start-thread thread 4)
  (yield)
  (displaynl "Ending thread2"))

(start-thread thread 1)
(start-thread thread2)
(start-thread thread 3)
(scheduler)