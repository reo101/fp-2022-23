#lang r5rs

(define (str-chr? str c)
    (letrec ((str-chr-acc 
        (lambda (str c i)
            (if (< i 0) 
                #f
                (or (eq? (string-ref str i) c) 
                    (str-chr-acc str c (- i 1)))))))
        (str-chr-acc str c (- (string-length str) 1))))

(define (str-sub str a b)
    (letrec ((str-sub-acc 
        (lambda (str a b curr acc)
            (cond
                ((< curr a) (str-sub-acc str a b (+ curr 1) acc))
                ((< b curr) acc)
                (else       (str-sub-acc str a b (+ curr 1) (string-append acc (make-string 1 (string-ref str curr)))))))))
        (str-sub-acc str (max a 0) (min b (- (string-length str) 1)) 0 (make-string 0))))

(define (str-str? needle haystack)
    (letrec 
      ( (nlen (string-length needle)) 
        (hlen (string-length haystack))
        (str-str-acc 
            (lambda (i l)
                (cond
                    ((>= l nlen) (- i nlen))
                    ((>= i hlen) #f)
                    ((eq? (string-ref needle l) (string-ref haystack i)) 
                        (str-str-acc (+ i 1) (+ l 1)))
                    (else (str-str-acc (+ i 1) 0)))))) 
      (str-str-acc 0 0)))