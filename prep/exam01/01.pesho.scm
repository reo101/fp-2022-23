#lang racket

;; a)
(define (accumulate op nv a b term next)
  (if (> a b)
    nv
    (op (term a) (accumulate op nv (next a) b term next))))

(define done?
  (lambda (x) 
    (let ((acc (accumulate + 0 1 (quotient x 2) 
                           (lambda (y)
                             (if (= 0 (remainder x y))
                               y
                               0))
                           (lambda (y) (+ y 1)))))
     (= acc (+ x 2)))))
      
;; b) [a, gotino1, x, gotino2, b]

;; (gotino2 - gotino1) + P(a, gotino1) + P(b , gotino2)

;; (P a gotino1) = (a + gotino1) / 2
;; (P gotino2 b) = (b + gotino2) / 2

;; a | gotino1   gotino2  |  b

;; 153
(define helper1
  (lambda (a b)
    (if (= 0 (remainder (+ a b) 2))
      (- (quotient (+ a b) 2) 1)
      (quotient (+ a b) 2))))

(define (get-list a b)
  (define (helper2 x) 
    (if (done? x) (list x) '()))
  (accumulate append '() a b helper2 (lambda (y) (+ y 1))))

(define (sum-almost-done a b)
  (let* ((spisuk (get-list a b)) 
         (gotino1 (car spisuk))
         (gotino2 (car (reverse spisuk))))
    (- (quotient (* (helper1 gotino2 b) (+ (helper1 gotino2 b) 1)) 2) 
       (quotient (* (helper1 gotino1 a) (+ (helper1 gotino1 a) 1)) 2))))
   
  
