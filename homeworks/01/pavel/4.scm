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

(define n-rooks
  (lambda (board n)
    (let* ((and (lambda (x y) (and x y)))
           (1+  (lambda (x)   (+ x 1)))
           (only-lone-rows
              (accumulate
                and
                #t
                0
                (- n 1)
                (lambda (row)
                  (= 1
                     (accumulate
                      +
                      0
                      0
                      (- n 1)
                      (lambda (col)
                        (if (board row col n)
                          1
                          0))
                      1+)))
                1+))
           (only-lone-cols
              (accumulate
                and
                #t
                0
                (- n 1)
                (lambda (col)
                  (= 1
                     (accumulate
                      +
                      0
                      0
                      (- n 1)
                      (lambda (row)
                        (if (board row col n)
                          1
                          0))
                      1+)))
                1+)))
      (and only-lone-rows only-lone-cols))))

;; ;; Testing
;;
;; (define (board1 x y n)
;;   (= (remainder (+ x 2) n) y))
;; (define (board2 x y n)
;;   (= (min (+ x 2) (- n 1)) y))
;;
;; (n-rooks board1 5) ;; -> #t
;; (n-rooks board2 5) ;; -> #f
