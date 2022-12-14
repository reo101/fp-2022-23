#lang r5rs

;; checks whether the list l2 begins with the list l1, 
;; where elementwise equality is given by the op procedure
(define (prefix-op? op l1 l2)
    (or (null? l1)
        (and (<= (length l1) (length l2))
             (op (car l1) (car l2)) 
             (prefix-op? op (cdr l1) (cdr l2)))))

;; checks whether the list l1 is a sublist of the list l2
;; where elementwise equality is given by the op procedure
(define (sublist-op? op l1 l2)
    (cond ((null? l1) #t)
          ((null? l2) #f)
          (else (or (prefix-op? op l1 l2) 
                    (sublist-op? op l1 (cdr l2))))))
                    
;; standard foldl function
(define (foldl op nv l)
    (if (null? l) 
        nv
        (foldl op (op nv (car l)) (cdr l))))

;; map consecutive pairs to the result of the binary procedure op applied over them
;; eg: (slide-pair-map + '(1 2 3)) -> '(3 5)
(define (slide-pair-map op lst)
    (if (< (length lst) 2) 
        (list)
        (cons (op (car lst) (cadr lst)) (slide-pair-map op (cdr lst)))))

;; finally, solve the initial problem by first slide-mapping consecutive lists to check majors
;; and then folding to check if all results are true (which we want to check)
(define (is-majored-by? l1 l2)
    (sublist-op? <= l1 l2))

(define (is-major? ll)
    (foldl (lambda (x y) (and x y)) 
           #t 
           (slide-pair-map is-majored-by? ll)))

;;;;;;;;;;;;;;;;
;; BONUS PART ;;
;;;;;;;;;;;;;;;;

(define (longer l1 l2)
    (if (>= (length l1) (length l2)) l1 l2))

(define (find-longest-major ll)
    (letrec 
        ((find-longest-major-acc 
            (lambda (ll curr most)
                (let ((curr-len (length curr))
                        (most-len (length most))) 
                        (if (null? ll) 
                            (longer curr most)
                            (let ((first (car ll)) 
                                  (rest  (cdr ll)))
                                 (if (or (null? curr) 
                                         (is-majored-by? (car curr) first))
                                     (find-longest-major-acc 
                                         rest (cons first curr) most)
                                     (find-longest-major-acc 
                                         rest (list first) (longer curr most)))))))))
        (reverse (find-longest-major-acc ll '() '()))))