#lang r5rs

(define (length l)
    (if (null? l ) 0 (+ 1 (length (cdr l)))))

(define (sum l) 
    (if (null? l) 0 (+ (car l) (sum (cdr l)))))

(define (last l)
    (cond 
        ((null? l)          "error brat")
        ((null? (cdr l))    (car l))
        (else               (last (cdr l)))))

(define (append l1 l2)
    (if (null? l1) 
        l2 
        (cons (car l1) (append (cdr l1) l2))))

(define (push-back l x)
    (if (null? l) 
        (list x)
        (cons (car l) (push-back (cdr l) x))))

(define (member? x l)
    (cond 
        ((null? l)          #f)
        ((equal? (car l) x) #t)
        (else               (member? x (cdr l)))))

(define (from-to a b)
    (if (> a b) 
        (list)
        (cons a (from-to (+ 1 a) b))))

(define (reverse l)
    (letrec 
      ((reverse-acc 
        (lambda (l acc) 
          (if (null? l) 
            acc
            (reverse-acc (cdr l) (cons (car l) acc)))))) 
        (reverse-acc l (list))))

(define (map f l)
    (if (null? l) 
        (list)
        (cons (f (car l)) (map f (cdr l)))))

(define (filter p? l)
    (cond 
      ((null? l) (list))
      ((p? (car l)) (cons (car l) (filter p? (cdr l))))
      (else (filter p? (cdr l)))))

(define (partition p? l)
  (cons 
    (filter p? l) 
    (filter 
      (lambda (x) (not (p? x))) 
      l)))

(define (take n l)
    (let ((len (length l))) 
    (cond 
        ((null? l) (list))
        ((= n 0) (list))
        ((< n 0) (drop (+ n len) l))
        ((>= n len) l)
        (else (cons (car l) (take (- n 1) (cdr l)))))))

(define (drop n l)
  (let 
    ((len (length l))) 
    (cond 
        ((null? l) (list))
        ((= n 0) l)
        ((< n 0) (take (+ n len) l))
        ((>= n len) (list))
        (else (drop (- n 1) (cdr l))))))

(define (all? p? l)
    (if (null? l) #t (and (p? (car l)) (all? p? (cdr l)))))

(define (any? p? l)
    (not (all? (lambda (x) (not (p? x))) l)))

; 17.
(define (remove x l)
    (cond 
        ((null? l) (list))
        ((equal? x (car l)) (cdr l))
        (else (cons (car l) (remove x (cdr l))))))

