#lang racket

(define (partial f . args)
    (lambda args-rest (apply f (append args args-rest))))

(define-syntax my-delay
    (syntax-rules () 
        ((my-delay item) (lambda () item))))

(define (my-force fitem) 
    (fitem))

(define-syntax strm
    (syntax-rules () ((strm x xs) (cons x (my-delay xs)))))

(define strm-car car)
(define (strm-cdr xs)
    (my-force (cdr xs)))
(define (id x) x)

;; nats
(define (nats-after n)
    (strm n (nats-after (+ n 1))))

(define nats (nats-after 0))

;; zada4ki
(define (strm-take n xs)
    (if (or (null? xs) (= n 0))
        '()
        (cons (strm-car xs) (strm-take (- n 1) (strm-cdr xs)))))

(define (strm-map f xs)
    (if (null? xs) 
        '()
        (strm (f (strm-car xs)) (strm-map f (strm-cdr xs)))))


(define (strm-filter p? xs)
    (cond 
        ((null? xs) '())
        ((p? (strm-car xs)) (strm (strm-car xs) (strm-filter p? (strm-cdr xs))))
        (else (strm-filter p? (strm-cdr xs)))))
        ; (else ((if  (p? (strm-car xs)) 
        ;             (lambda (rest) 
        ;                 (strm (strm-car xs) (my-force rest))) 
        ;             my-force)

        ;         (my-delay (strm-filter p? (strm-cdr xs)))))))

(define (prime? n)
    (null? (filter (lambda (i) (= 0 (remainder n i))) (range 2 n))))

(define primes (strm-filter prime? (nats-after 2)))

(define (sieve rest)
  (strm (strm-car rest) 
    (sieve 
      (strm-filter 
        (lambda (n) 
          (not (= 0 (remainder n (strm-car rest))))) 
        (strm-cdr rest)))))

(define primes2 (sieve (nats-after 2)))

(define (iterate f x)
    (strm x (strm-map f (iterate f x))))