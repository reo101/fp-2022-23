#lang racket

;; 1
(define (len l)
    (if (null? l) 
        0 
        (+ 1 (len (cdr l)))))

;; 2
(define (exists? l p)
    (cond 
        ((null? l) #f)
        ((p (car l)) #t)
        (else (exists? (cdr l) p))))

;; 3
(define (member? l x)
    (cond
        ((null? l) #f)
        ((equal? (car l) x) #t)
        (else (member? (cdr l) x))))

;; 4
(define (at n l)
    (cond 
        ((or (< n 0) (null? l))  #f)
        ((= n 0)    (car l))
        (else       (at (- n 1) (cdr l)))))

;; 5
(define (map f l)
    (cond 
        ((null? l) l)
        (else (cons (f (car l)) (map f (cdr l))))))

;; 6
(define (filter p l)
    (let 
        ((cons-if 
            (lambda (p x l)
                (if (p x) (cons x l) l))))
        (if (null? l) 
            l
            (cons-if p (car l) (filter p (cdr l))))))

;; 7
(define (push x l)
    (if (null? l) 
        (cons x (list))
        (cons (car l) (push x (cdr l)))))

;; 8
(define (reverse l)
    (if (null? l)
        l
        (push (car l) (reverse (cdr l)))))

;; 9
(define (insert x n l)
    (cond
        ((< n 0)    (insert x (+ n 1 (length l)) l))
        ((null? l)  (list x))
        ((= n 0)    (cons x l))
        (else       (cons (car l) (insert x (- n 1) (cdr l))))))

;; 10
(define (foldr l op init)
    (if (null? l) 
        init
        (op (car l) (foldr (cdr l) op init))))

;; 11
(define (sum l)
    (foldr l + 0))