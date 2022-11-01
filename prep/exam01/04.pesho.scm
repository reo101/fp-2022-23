#lang racket

(define (accumulate op nv a b term next)
  (if (> a b)
    nv
    (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-list op nv l term)
  (if (null? l)
    nv
    (op (term (car l)) (accumulate-list op nv (cdr l) term))))

(define (cool-remainder x)
  (remainder x 10))

(define (product-digits x)
  (define (helper num acc)
    (if (= 0 num)
      acc
      (helper (quotient num 10) (* acc (remainder num 10)))))
  (helper x 1))

(define (special-diff n)
  (- n (product-digits n)))

(define (special-diff-term pair)
  (- (special-diff (car pair)) (special-diff (cdr pair))))

(define (prepend-to-all value y)
  (if (null? y)
      '()
      (cons (cons value (car y)) (prepend-to-all value (cdr y)))))

(define (map2d x y)
  (if (null? x)
      '()
      (append (prepend-to-all (car x) y) (map2d (cdr x) y))))

(define (generate-list a b)
  (if (> a b) 
    '()
    (append (list a) (generate-list (+ a 1) b))))

(define (max a b)
  (if (> a b)
    a
    b))

(define (largest-diff a b)
  (accumulate-list max -1 (map2d 
                            (generate-list a b) 
                            (generate-list a b)) special-diff-term))
