#lang r5rs

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (foldl op nv l)
  (if (null? l)
      nv
      (foldl op (op nv (car l)) (cdr l))))

(define zip
  (lambda (l1 l2)
    (letrec
        ((zip-acc
          (lambda (l1 l2 acc)
            (if (or (null? l1)
                    (null? l2))
                acc
                (zip-acc (cdr l1)
                         (cdr l2)
                         (cons (cons (car l1)
                                     (car l2))
                               acc))))))
      (zip-acc l1 l2 '()))))

(define map
  (lambda (f l)
    (if (null? l)
        l
        (cons (f (car l))
              (map f (cdr l))))))

(define (is-majored-by? l1 l2)
  (if (< (length l2)
         (length l1))
      #f
      (letrec
        ((mini-majors
          (map (lambda (pair)
                 (<= (car pair)
                     (cdr pair)))
               (zip l1 l2))))
        (foldl (lambda (raz dva)
                 (and raz dva))
               #t
               mini-majors))))

(define (sub-major? l1 l2)
  (cond ((null? l1) #t)
        ((null? l2) #f)
        (else (or (is-majored-by? l1 l2)
                  (sub-major? l1 (cdr l2))))))

(define (is-major? ll)
  (if (< (length ll) 2)
    #t
    (and (sub-major? (car  ll)
                     (cadr ll))
        (is-major? (cdr ll)))))

;; (is-major? '((1 3) (4 2 7) (2 5 4 3 9 12))) → #t
;; (is-major? '((1 3) (4 2 7) (2 5 3 3 9 12))) → #f

(define find-longest-major
  (lambda (ll)
    (letrec
      ((find-longest-major-acc
         (lambda (ll curr most)
           (if (< (length ll) 2)
             (if (< (length curr)
                    (length most))
               most
               curr)
             (if (sub-major? (car  ll)
                             (cadr ll))
               (find-longest-major-acc
                 (cdr ll)
                 (cons (car ll)
                       curr)
                 most)
               (find-longest-major-acc
                 (cdr ll)
                 '()
                 (if (< (+ 1 (length curr))
                        (length most))
                   most
                   (cons (car ll)
                         curr))))))))
      (reverse (find-longest-major-acc ll '() '())))))

;; (find-longest-major '((1) (2) (1) (2) (3) (1) (2))) → '((1) (2) (3))
