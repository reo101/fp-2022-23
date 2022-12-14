#lang racket

(define head (lambda (x) (car x)))
(define tail (lambda (x) (cdr x)))

;;(define (map f l)
;;  (if (null? l) '()
;;    (cons (f (head l)) (map f (tail l))))

(define (map f l)
  (cond
    [(null? l) '()]
    [(number? (head l)) (append (list (f (head l))) (map f (tail l)))]
    [else (append (list (head l)) (map f (tail l)))]))
    

(define (helper2 f stack n)
  (if (or (= n 0) (null? stack) (symbol? (head stack)) (symbol? (head (tail stack)))) 
    stack
    (helper2 f (append (list (f (head stack) (head (tail stack)))) (tail (tail stack))) (- n 1))))
  
(define (run-machine ll)
  (define (helper ll stack)
    (if (null? ll) stack
      (cond 
        [(or (number? (head ll)) (symbol? (head ll)))
         (helper 
           (tail ll) 
           (append stack (list (head ll))))]
        [(procedure? (head ll)) 
         (helper (tail ll) (map (head ll) stack))]
        ;; not here
        [(pair? (head ll)) 
         (helper (tail ll) (reverse (helper2 (head (head ll)) (reverse stack) (tail (head ll)))))]
                                    
        [else (helper (tail ll) stack)])))
  (helper ll '()))
