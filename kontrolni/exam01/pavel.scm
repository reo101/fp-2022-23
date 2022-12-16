#lang r5rs

(define (accumulate op nv a b term next)
  (if (> a b)
    nv
    (op (term a)
        (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b)
    nv
    (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (foldr op nv l)
  (if (null? l)
    nv
    (op (car l)
        (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l)
    nv
    (foldl op (op nv (car l)) (cdr l))))

(define (filter p? l)
  (foldr (lambda (x r)
           (if (p? x)
             (cons x r)
             r))
         '()
         l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1

;; (trim 360) -> 12

(define from-to
  (lambda (a b)
    (if (> a b)
      '()
      (cons a
            (from-to (+ 1 a)
                     b)))))

(define divisors
  (lambda (n)
    (filter
      (lambda (x)
        (= 0
           (remainder n
                      x)))
      (from-to 1 n))))

(define prime?
  (lambda (n)
    (= 2
       (length
         (divisors n)))))

(define prime-divisors
  (lambda (n)
    (filter prime? (divisors n))))

(define (trim n)
  (apply / n (prime-divisors n)))

;;; 2

;; (commonUnitary 60 140) -> 4

(define intersect
  (lambda (l1 l2)
    (filter (lambda (x)
              (member x l2))
            l1)))

(define is-a-unitary-divisor
  (lambda (n k)
    (null? (intersect (prime-divisors (/ n k))
                      (prime-divisors k)))))

(define unitaryDivisors
  (lambda (n)
    (filter (lambda (x)
              (is-a-unitary-divisor n x))
            (divisors n))))

(define commonUnitary
  (lambda (n1 n2)
    (length (intersect (unitaryDivisors n1)
                       (unitaryDivisors n2)))))

;;; 3

;; (selectiveMerge * '(1 2 3 4 1 3 1 2) '(10 1 2 0 5 -2 -1 4)) -> '(1 2 6 0 5 3 1 8)

;; rule 1 is #t, rule 2 is #f
(define rules
  (lambda (fab la lb)
    (define rules-acc
      (lambda (fab la lb acc)
        (if (or (null? la)
                (null? lb))
          acc
          (let* ((a   (car la))
                 (as  (cdr la))
                 (b   (car lb))
                 (bs  (cdr lb))
                 (new (cond ((< (fab a b)
                                (min a b))
                             #t)
                            ((> (fab a b)
                                (max a b))
                             #f)
                            (else (car acc)))))
            (rules-acc fab as bs (cons new acc))))))
    (reverse
      (rules-acc
        fab
        (cdr la)
        (cdr lb)
        '(#t)))))

(define selectiveMerge
  (lambda (fab la lb)
    (define selectiveMerge-acc
      (lambda (rules la lb acc)
        (if (or (null? la)
                (null? lb))
          acc
          (let ((rule (car rules))
                (rest (cdr rules))
                (a    (car la))
                (as   (cdr la))
                (b    (car lb))
                (bs   (cdr lb)))
            (selectiveMerge-acc
              rest
              as
              bs
              (cons (if rule
                      a
                      (fab a b))
                    acc))))))
    (reverse
      (selectiveMerge-acc
        (rules fab la lb)
        la
        lb
        '()))))

;;; 4

;; (preferredNetwork '(1 3 5 7 8 20) '((1 3 8 40 41) (1 3 7 28) (5))) -> '(1 3 7)

(define max-on
  (lambda (f l)
    (define max-on-acc
      (lambda (f l acc)
        (if (null? l)
          acc
          (let* ((head (car l))
                 (tail (cdr l))
                 (new-acc (if (f head acc) head acc)))
            (max-on-acc f tail new-acc)))))
    (max-on-acc f (cdr l) (car l))))

(define join
  (lambda (l1 l2)
    (foldl (lambda (acc x)
             (if (member x l2)
               acc
               (cons x acc)))
           l2
           l1)))

(define coverage
  (lambda (phone network)
    (/ (length (intersect phone network))
       (length network))))

(define preferredNetwork
  (lambda (phone networks)
    (let* ((networks-with-coverage
             (map (lambda (network)
                    (cons (coverage phone network)
                          network))
                  networks))
           (good-networks-with-coverage
             (filter (lambda (network-with-coverage)
                       (and (not (= (car network-with-coverage)
                                    0))
                            (>= (length (intersect phone
                                                   (cdr network-with-coverage)))
                                2)))
                     networks-with-coverage))
           (better-network-with-coverage
             (lambda (network-with-coverage1 network-with-coverage2)
               (> (car network-with-coverage1)
                  (car network-with-coverage2))))
           (best-network
             (if (= (length good-networks-with-coverage)
                    0)
               '()
               (cdr (max-on better-network-with-coverage
                            good-networks-with-coverage)))))
      (intersect phone best-network))))

;;; bonus

;; (preferredNetworkForAll '((1 3) (1 2) (2 3)) '((1 2) (1 2 3))) -> '(1 2)

;; '(1 2)   has coverages '(___ 2/3 2/3) -> total = 4/3
;; '(1 2 3) has coverages '(2/3 2/3 2/3) -> total = 6/3

;; phone-with-coverage-for-network        -> '(coverage . (freq1 freq2 ...))
;; good-phone-with-coverage-for-network   -> same, but (not (= coverage 0))
;; network-with-good-phones-with-coverage -> '(network . (good-phone-with-coverage-for-network1
;;                                                        good-phone-with-coverage-for-network2 ...))))

(define preferredNetworkForAll
  (lambda (phones networks)
    (let* ((phones-with-coverage-for-network
             (lambda (network)
               (map (lambda (phone)
                      (cons (coverage phone network)
                            phone))
                    phones)))
           (good-phones-with-coverage-for-network
             (lambda (phone)
               (filter (lambda (phone-with-coverage)
                         (and (not (= (car phone-with-coverage)
                                      0))
                              (>= (length (intersect phone
                                                     (cdr phone-with-coverage)))
                                  2)))
                       (phones-with-coverage-for-network phone))))
           (networks-with-good-phones-with-coverage
             (map (lambda (network)
                    (cons network
                          (good-phones-with-coverage-for-network network)))
                  networks))
           (better-network-with-good-phones-with-coverage
             (lambda (network-with-good-phones-with-coverage1 network-with-good-phones-with-coverage2)
              (let ((total-coverage
                      (lambda (network-with-good-phones-with-coverage)
                        (foldl
                          (lambda (acc x)
                            (+ acc
                               (car x)))
                          0
                          (cdr network-with-good-phones-with-coverage)))))
                (if (> (total-coverage network-with-good-phones-with-coverage1)
                       (total-coverage network-with-good-phones-with-coverage2))
                  network-with-good-phones-with-coverage1
                  network-with-good-phones-with-coverage2))))
           (best-coverage-network
             (car (foldl better-network-with-good-phones-with-coverage
                         '(() . ())
                         networks-with-good-phones-with-coverage)))
           (best-frequencies
             (filter
               (lambda (frequency)
                 (foldl (lambda (acc phone) (or acc
                                                (member frequency phone)))
                        #f
                        phones))
               best-coverage-network)))
      best-frequencies)))
