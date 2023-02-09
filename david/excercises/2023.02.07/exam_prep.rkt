#lang racket

(define (leap? y)
  (or 
    (zero? (remainder y 400)) 
    (and 
      (not (zero? (remainder y 100)))
      (zero? (remainder y 4)))
    ))

(define (days-in-month m y)
  (case m
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (else (if (leap? y) 29 28))))


;; accum

(define (id x) x)
(define (1+ n) (+ 1 n))


(define (accumulate op nv a b term next)
  (if (> a b) nv
    (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
    (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (fact n)
  (accumulate-i * 1 1 n id 1+))

(define (monom x n)
  (accumulate-i * 1 1 n (lambda (i) x) 1+))

(define (taylor-exp n x)
  (exact->inexact (accumulate-i + 0 0 n (lambda (i) (/ (monom x i) (fact i))) 1+)))

(define (any p a b)
  (accumulate-i (lambda (u v) (or u v)) #f a b p 1+))

(define (maximum x . rest)
  (if (null? rest) 
    x 
    (max x (apply maximum rest))))

;; foldove

(define (foldr op nv l)
  (if (null? l)
    nv
    (op (car l) (foldr op nv (cdr l)))))

(define (foldr1 op l)
  (if (null? (cdr l))
    (car l)
    (op (car l) (foldr1 op (cdr l)))))

(define (maximum2 x . rest)
  (foldr1 max (cons x rest)))

(define-syntax guard
  (syntax-rules () ((guard b x) (if b x #f))))

(define maybe-head (lambda l (guard (not (null? l)) (car l))))

(define (cons#f h t) (and t (cons h t)))

(define-syntax stream-cons
  (syntax-rules () 
  ((stream-cons x xs) 
    (cons x (delay xs)))))

(define-syntax stream
  (syntax-rules 
    ()
    ((stream l)
      (if (null? l)
      (list)
      (stream-cons (car l) (cdr l))))))

(define (const x) (stream-cons x (const x)))