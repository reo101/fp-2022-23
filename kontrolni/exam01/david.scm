#lang r5rs

;;;;;;;;;;;;;;;;;
;;;; GIVENS  ;;;;
;;;;;;;;;;;;;;;;;

(define (foldl op nv l)
    (if (null? l) 
        nv 
        (foldl op (op nv (car l)) (cdr l))))

(define (foldr op nv l)
    (if (null? l) 
        nv 
        (op (car l) (foldr op nv (cdr l)))))

(define (filter p? l)
    (foldr 
        (lambda (x r) 
            (if (p? x) (cons x r) r)) 
        (list) 
        l))

(define (accumulate op nv a b term next)
    (if (> a b) nv 
        (op (term a)
            (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

;;;;;;;;;;;;;;;;;
;;;; HELPERS ;;;;
;;;;;;;;;;;;;;;;;

(define (1+ n) (+ 1 n))
(define (id x) x)

(define (elem? x l)
    (foldl 
        (lambda (acc y) (or acc (= x y))) 
        #f 
        l))

(define (all p? l)
    (foldl (lambda (acc x) (and acc (p? x))) #t l))

(define (any p? l)
    (not (all (lambda (x) (not (p? x))) l)))

(define (count p? l)
    (length (filter p? l)))

(define (map f l)
    (if (null? l)
        (list)
        (cons (f (car l)) (cdr l))))

(define (argmax comp l)
    (foldr 
        (lambda (x acc) 
            (if (> (comp x) 
                   (comp acc)) 
                x 
                acc))
        (car l)
        l))

;;;;;;;;;;;;;;;;;
;;;; TASK 01 ;;;;
;;;;;;;;;;;;;;;;;

(define (factors n)
    (accumulate 
        (lambda (x acc) (if (= 0 (remainder n x)) (cons x acc) acc)) 
        '() 
        1 
        n 
        id 
        1+))

(define (prime? n)
    (equal? (factors n) (list 1 n)))

(define (prime-factors n)
    (filter prime? (factors n)))

(define (trim n)
    (foldl (lambda (x y) (quotient x y)) n (prime-factors n)))

;;;;;;;;;;;;;;;;;
;;;; TASK 02 ;;;;
;;;;;;;;;;;;;;;;;

(define (coprime? n k )
    (let 
        ((prime1 (prime-factors n))
         (prime2 (prime-factors k)))
        (all (lambda (x) (not (elem? x prime1))) prime2)))

(define (unitary-divisor? n k)
    (and (= 0 (remainder n k)) 
         (coprime? k (quotient n k))))

(define (unitary-divisors n)
    (accumulate
        (lambda (x acc) 
            (if (unitary-divisor? n x) 
                (cons x acc) 
                acc)) 
        '() 1 n id 1+))

(define (commonUnitary n1 n2)
    (let 
        ((unitary1 (unitary-divisors n1))
         (unitary2 (unitary-divisors n2)))
    
        (count (lambda (x) (elem? x unitary1)) unitary2)))

;;;;;;;;;;;;;;;;;
;;;; TASK 03 ;;;;
;;;;;;;;;;;;;;;;;

;;basically a stateful map on a list of pairs, 
;; which remembers the previously applied combiner (a function f: (a . b) -> (f a b))
(define (merge f previous-combiner pairs)
    (if (null? pairs) 
        pairs
        (let* 
            ((a (caar pairs))
             (b (cdar pairs))
             (res (f a b))
             (combiner (cond 
                ((< res (min a b)) 
                    (lambda (a b) a))
                ((> res (max a b)) 
                    (lambda (a b) (f a b)))
                (else previous-combiner)
                ))) 
        
            (cons (combiner a b) (merge f combiner (cdr pairs))))))

;;zipper function
(define (zip l1 l2)
    (if (or (null? l1) (null? l2))
        (list)
        (cons 
            (cons (car l1) (car l2)) 
            (zip  (cdr l1) (cdr l2)))))

(define (selectiveMerge f as bs)
    (merge f 
        (lambda (a b) a) ;; initially we combine with const
        (zip as bs)))

;;;;;;;;;;;;;;;;;
;;;; TASK 04 ;;;;
;;;;;;;;;;;;;;;;;

(define (common phone net)
    (filter (lambda (x) (elem? x phone)) net))

(define (common-len phone net)
    (length (common phone net)))

(define (compatible? phone net)
    (<= 2 (common-len phone net)))

(define (coverage phone net)
    (/ (common-len phone net) (length net)))

(define (preferredNetwork phone nets)
    (let* 
        ((compatibles 
            (filter 
                (lambda (net) 
                    (compatible? phone net)) 
            nets))
         (best 
            (argmax 
                (lambda (net) 
                    (coverage phone net)) 
            compatibles)))

        (common phone best)))

;;;;;;;;;;;;;;;;;
;;;; TASK 05 ;;;;
;;;;;;;;;;;;;;;;;

(define (count-compatibles net phones)
    (count (lambda (phone) (compatible? phone net)) phones))

;; basically max after map
(define (maximum-phones nets phones)
    (foldr
        (lambda (net acc) 
            (max acc 
                 (count-compatibles net phones))) 
        0 
        nets))

(define (sumCoverages net phones)
    (foldl (lambda (acc phone) (+ acc (coverage phone net))) 0 (filter (lambda (phone) (compatible? phone net)) phones)))

(define (preferredNetworkForAll phones nets)
    (let* 
      (
        ;; get the maximum number of phones a net is compatible with
        (maximumPhones 
            (maximum-phones nets phones)) 
        ;;get all the nets with that maximum number of compatible phones
        (bestNets 
            (filter 
                (lambda (net) 
                    (= maximumPhones 
                       (count-compatibles net phones))) 
                nets)) 
        ;;of those best ones, get the one with maximum sum of phone coverages
        (bestNet 
            (argmax 
                (lambda (net) 
                    (sumCoverages net phones)) 
                bestNets))
        ;;get all the covered phones by that network
        (bestNetPhones 
            (filter 
                (lambda (phone) 
                    (compatible? phone bestNet)) 
                phones))
        ;;and get all the bandwiths that are common with at least one phone
        (commonLines 
            (filter 
                (lambda (line) 
                    (any 
                        (lambda (phone) 
                            (elem? line phone)) 
                        bestNetPhones)) 
                bestNet)))
      commonLines))
