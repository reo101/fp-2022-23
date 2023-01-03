#lang racket

;; 1
(define (at m i j) 
    (list-ref (list-ref m i) j))

;; 2
(define (mat-map f m)
    (map (lambda (l) (map f l)) m))

;; 3
(define (all? p l)
    (foldl (lambda (el acc) (and acc (p el))) true l))

(define (mat? m)
    (and 
        (list? m)
        (all? list? m)
        (all? number? (flatten m))
        (all? (lambda (x) (= x (length (car m)))) (map length m))))

;; 4
(define (scalmul x m) 
    (mat-map (lambda (a) (* a x)) m))

;; 5
(define (transpose m)
    (if (or (null? m) (all? null? m)) 
        '()
        (cons (map car m) (transpose (map cdr m)))))

;; 6
(define (dim m)
    (if (not (mat? m))
        false
        (cons (length m) (length (car m)))))

(define (scalar-prod u v)
    (if (not (= (length u) (length v)))
        false
        (foldl + 0 (map * u v))))

(define (matmul m n)
    (if (and (dim m) (dim n) (= (cdr (dim m)) (car (dim n))))
        (map 
            (lambda (mRow)
                (map 
                    (lambda (nCol) 
                        (scalar-prod mRow nCol)) 
                    (transpose n))) 
            m)
        false))