#lang r5rs

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b)
      nv
      (accumulate op (op nv (term a)) (next a) b term next)))

;; (define done?
;;   (letrec
;;       ((divides? (lambda (a b)
;;                    (= 0 (remainder a b))))
;;        (head car)
;;        (tail cdr)
;;        (filter (lambda (xs p?)
;;                  (cond ((null? xs)     '())
;;                        ((p? (head xs)) (cons
;;                                         (head xs)
;;                                         (filter (tail xs) p?)))
;;                        (else           (filter (tail xs) p?)))))
;;        (n-to-m (lambda (n m)
;;                  (if (> n m)
;;                      '()
;;                      (cons n (n-to-m (+ n 1) m)))))
;;        (n-is-divided-by? (lambda (n)
;;                            (lambda (x)
;;                              (divides? n x))))
;;        (divisors (lambda (n)
;;                    (filter (n-to-m 1 (- n 1))
;;                            (n-is-divided-by? n))))
;;        (sum (lambda (xs)
;;               (if (null? xs)
;;                   0
;;                   (+ (head xs)
;;                      (sum (tail xs)))))))
;;     (lambda (n)
;;       (= (+ n 2)
;;          (sum (divisors n))))))

(define done?
  (lambda (n)
    (= (+ n 2)
       (accumulate
         + 0 1 (quotient n 2)
         (lambda (x)
           (if (= 0 (remainder n x)) x 0))
         (lambda (x) (+ x 1))))))

;; (done? 20) ;; #t
;; (done? 28) ;; #f

(define sum-almost-done
  (lambda (a b)
    (letrec
      ((id
         (lambda (x) x))
       (succ
         (lambda (x) (+ x 1)))
       (cons-if-done
         (lambda (x xs)
           (if (done? x)
             (cons x xs)
             xs)))
       (dones
         (accumulate
           cons-if-done
           '()
           a b
           id succ))
       (first-done
         (lambda (l)
           (if (null? l)
             0
             (car dones))))
       (last-done
         (lambda (l)
           (cond
             ((null? l)       0)
             ((null? (cdr l)) (car l))
             (#t              (last-done (cdr l))))))
       (first-closest-to-done
         (ceiling (/ (+ 1 a (first-done dones)) 2)))
       (last-closest-to-done
         (floor   (/ (+ -1 b (last-done dones)) 2))))
      (accumulate
        + 0
        first-closest-to-done
        last-closest-to-done
        id
        succ))))

;; (sum-almost-done 5 24) ;; 153
