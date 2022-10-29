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

(define (id x) x)
(define (succ n) (+ n 1))

(define (done? n) 
    (let ((succ (lambda (i) (+ i 1)))
          (divisor-or-zero (lambda (i) (if (= (remainder n i) 0) i 0))))
        (=  (+ n 2)
            (accumulate + 0 1 (quotient n 2) divisor-or-zero succ))))

(define (last-elem list)
    (if 
        (null? list) 
        list
        (if (null? (cdr list)) (car list) (last-elem (cdr list)))))

(define (sum-til-n n) 
    (quotient (* n (succ n)) 2))

(define (sum-almost-done a b)
    (let* (
        (dones (accumulate (lambda (n so-far) (if (done? n) (cons n so-far) so-far)) '() a b id succ))
        (fst (car dones))
        (last (last-elem dones)))
        
        ;; constant complexity body
        (- (sum-til-n (quotient (+ -1 b last) 2))
            (sum-til-n (quotient (+ a fst) 2)))
        

        ;; linear complexity body
        ;(accumulate + 0 (quotient (+ 1 a fst) 2) (quotient (+ -1 b last) 2) id succ)
        
        ))