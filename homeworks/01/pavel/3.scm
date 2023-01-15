#lang r5rs

(define (accumulate op nv a b term next)
  (if (> a b)
    nv
    (op (term a)
        (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b)
      nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define integrate1
  (lambda (f a b dx)
    (accumulate
      +
      0
      a
      b
      (lambda (x)
        (* (f x)
           dx))
      (lambda (x)
        (+ x dx)))))

(define integrate2
  (lambda (f a b c d dx dy)
    (accumulate
      +
      0
      c
      d
      (lambda (y)
        (* (integrate1
             (lambda (x)
               (f x y))
             a
             b
             dx)
           dy))
      (lambda (y)
        (+ y dy)))))

;; ;; Testing
;;
;; (define pi 3.14159265359)
;;
;; (define (f x y) (+ x (sin y) 1))
;;
;; (define res
;;   (/ (integrate2 f 0 2 (- pi) pi 0.001 0.001)
;;      pi))
