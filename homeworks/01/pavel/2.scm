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

(define id
  (lambda (x)
    x))

(define 1+
  (lambda (x)
    (+ x 1)))

(define divisors-count
  (lambda (n)
    (define is-divisor-generator
      (lambda (n)
        (lambda (x)
          (if (= (remainder n x)
                 0)
            1
            0))))

    (accumulate-i
      +
      0
      1 n
      (is-divisor-generator n)
      1+)))

(define fold-pair
  (lambda (op pair)
    (op (car pair)
        (cdr pair))))

(define better-pair
  (lambda (pair1 pair2)
    (let ((divisors-in-pair
            (lambda (p)
              (divisors-count (fold-pair + p)))))
      (if (>= (divisors-in-pair pair1)
              (divisors-in-pair pair2))
        pair1
        pair2))))

(define best-pair-for-number
  (lambda (a b1 b2)
    (accumulate-i
      better-pair
      (cons 0 1)
      b1 b2
      (lambda (b)
        (cons a b))
      1+)))

(define best-pair
  (lambda (a b)
    (accumulate-i
      better-pair
      (cons 0 1)
      a
      b
      (lambda (i)
        (best-pair-for-number i (+ i 1) b))
      1+)))
