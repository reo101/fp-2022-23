#lang racket

;;;;;;;;;;;;;;;;
;;;; GIVENS ;;;;
;;;;;;;;;;;;;;;; 

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

;;;;;;;;;;;;;;;;
;;; HELPERS ;;;;
;;;;;;;;;;;;;;;;

(define (1+ x)
    (+ x 1))

(define (id x) x)

(define (argcomp comp f x y)
    (if (comp (f x) (f y)) x y))

;;;;;;;;;;;;;;;;
;;;; TASK 1 ;;;;
;;;;;;;;;;;;;;;;

(define (argmin f a b)
    (let ((argless
            (lambda (x y)
                (argcomp < f x y))))
         (accumulate-i argless a a b id 1+)))

;;;;;;;;;;;;;;;;
;;;; TASK 2 ;;;;
;;;;;;;;;;;;;;;; 

(define (divides? d n)
    (= 0 (modulo n d)))

(define (count-divisors n)
  (let ((increment-if-divisor 
            (lambda (x acc) 
                ((if (divides? x n) 1+ id) acc))))
       (accumulate increment-if-divisor 0 1 n id 1+)))

(define (join-pair op pair)
    (op (car pair) (cdr pair)))

;; define the comparator, i.e. what a "better pair" means
;; for our task: (x1, y1) is better-than (x2, y2) 
;; if x1 + y1 has more divisors than x2 + y2
(define (better-pair p1 p2)
    (argcomp 
        >=  ;; replace with > if you want the highest x, 
            ;; i.e. (17, 19) instead of (16, 20) from the provided example
        (lambda (p) 
            (count-divisors 
                (join-pair + p))) 
        p1 
        p2))

;; finds the "best pair" (the pair (m, n) for which m + n has the most divisors) 
;; from the set {(x, i) | i <- [a, b]}
(define (most-mult x a b)
    (accumulate-i 
        better-pair 
        (cons 0 1) 
        a 
        b 
        (lambda (i) 
            (cons x i)) 
        1+))

;; essentially solves the task :)
(define (best-pair a b)
    (accumulate-i
        better-pair
        (cons 0 1)
        a
        b
        (lambda (i) 
            (most-mult i (+ i 1) b))
        1+))

;;;;;;;;;;;;;;;;
;;;; TASK 3 ;;;;
;;;;;;;;;;;;;;;; 

(define (integrate2 f a b c d dx dy)
    (accumulate-i + 0 c d
        (lambda (y) 
            (accumulate-i + 0 a b 
                (lambda (x)
                    (* (f x y) dx dy))
                (lambda (x) 
                    (+ x dx))))
        (lambda (y) 
            (+ y dy))))

;;;;;;;;;;;;;;;;
;;;; TASK 4 ;;;;
;;;;;;;;;;;;;;;; 

;; I've chosen Variant 1: boards as functions
(define (board1 x y n)
  (= (remainder (+ x 2) n) y))
(define (board2 x y n)
  (= (min (+ x 2) (- n 1)) y))

(define (n-rooks board n)
    (let* 
        ((target-sum (/ (* n (+ n 1)) 2))
         (add-pairs 
            (lambda (p1 p2)
                (cons (+ (car p1) (car p2)) 
                      (+ (cdr p1) (cdr p2)))))
         (on-board-or-zero 
            (lambda (pair)
                (if (board 
                        (- (car pair) 1) 
                        (- (cdr pair) 1) 
                        n) 
                    pair
                    (cons 0 0)))))
        (equal? 
            (cons target-sum target-sum) 
            (accumulate-i 
                add-pairs 
                (cons 0 0) 
                1
                n
                (lambda (i)
                    (accumulate-i 
                        add-pairs
                        (cons 0 0)
                        1
                        n
                        (lambda (j)
                            (on-board-or-zero (cons i j)))
                        1+))
                1+))))

;;;;;;;;;;;;;;;;;;;;;;
;;;; TASK 4 BONUS ;;;;
;;;;;;;;;;;;;;;;;;;;;;

;; Variant 2: boards as lists
;; A solution with foldl, which makes it a lot more concise
;; and better communicates the initial idea of the solution above

; (define (foldl op nv l)
;   (if (null? l) nv
;     (foldl op (op nv (car l)) (cdr l))))

; (define board1 (list (cons 0 2) (cons 1 3) (cons 2 4) (cons 3 0) (cons 4 1)))   ;; -> #t
; (define board2 (list (cons 0 2) (cons 1 3) (cons 2 4) (cons 3 2)))              ;; -> #f

; (define (n-rooks board n)
;   (let 
;     ((target-sum (/ (* n (+ n 1)) 2))
;      (add-pairs 
;        (lambda (p1 p2)
;           (cons (+ (car p1) (car p2) 1) 
;                 (+ (cdr p1) (cdr p2) 1)))))
;     (equal?
;       (cons target-sum target-sum)
;       (foldl add-pairs (cons 0 0) board))))