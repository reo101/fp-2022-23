#lang racket

(define (head l) (car l))
(define (tail l) (cdr l))

(define (accumulate op nv a b term next)
  (if (> a b)
    nv
    (op (term a) (accumulate op nv (next a) term next))))

(define (is-list-major? l1 l2)
  (define (helper l1 l2)
    (cond
      [(null? l1) #t]
      [(null? l2) #f]
      [(<= (head l1) (head l2))
       (helper (tail l1)
               (tail l2))]
      [else #f]))
  (helper l1 l2))

(define (gotin-or a b)
  (or a b))

(define (is-list-major2? l1 l2)
    (if (null? l2)
      #f
      (gotin-or (is-list-major? l1 l2) (is-list-major2? l1 (tail l2)))))
  

(define (gotin-and a b)
  (and a b))
  
(define (is-major? ll)
  (if (> 2 (length ll))
    #t
    (gotin-and (is-list-major2? 
                 (head ll) 
                 (head (tail ll))) 
               (is-major?
                 (tail ll)))))

;; Бонус

(define (find-longest-major ll)
  (define (helper ll ans curr prev)
    (if (= 1 (length ll))
      (if (is-list-major2? prev (head ll))
        (append ans (list (head ll)))
        ans)
      (if (is-list-major2? (head ll) (head (tail ll)))
        (helper (tail ll) (if (>= (length (append curr (list (head ll)))) (length ans)) 
                            (append curr (list (head ll))) ans) (append curr (list (head ll))) (head ll))
        (helper (tail ll) ans '() (head ll)))))
  (helper ll '() '() '()))
