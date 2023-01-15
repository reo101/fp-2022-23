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

(define argmin
  (lambda (f a b)
    (accumulate-i
      (lambda (x acc)
        (if (< (f x)
               (f acc))
          x
          acc))
      a
      (+ a 1)
      b
      (lambda (x)
        x)
      (lambda (x)
        (+ x 1)))))
