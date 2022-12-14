#lang r5rs

(define (product-digits n)
    (if (< n 10) n
        (* (remainder n 10) (product-digits (quotient n 10)))))

(define (canonical-product-digits n)
    (if (< n 10) n
        (canonical-product-digits (product-digits n))))

(define (diff-product-digits n)
    (- n (product-digits n)))

(define (generate-interval a b)
    (if (> a b) (list) (cons a (generate-interval (+ a 1) b))))

(define (map op lst)
    (if (null? lst) lst (cons (op (car lst)) (map op (cdr lst)))))

(define (foldl op nv l)
  (if (null? l) 
      nv
      (foldl op (op nv (car l)) (cdr l))))

(define (max a b)
    (if (> a b) a b))

(define (largest-diff a b)
    (letrec 
        ((diff-interval (map diff-product-digits (generate-interval (ceiling a) (floor b))))
         (largest-diff-acc (lambda (interval curr most)
             (if (null? interval) 
                 most
                 (largest-diff-acc 
                     (cdr interval) 
                     (car interval) 
                     (max
                         most
                         (foldl
                             (lambda (x y) (max (- y curr) x))
                             most
                             interval)))))))
        (if (null? diff-interval) 0 (largest-diff-acc (cdr diff-interval) (car diff-interval) 0))))