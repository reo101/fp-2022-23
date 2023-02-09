#lang racket

;; quicksort with constant leftmost pivot
(define (sort-by comp l)
  (if (null? l) l
    (letrec 
      ( (pivot (car l))
        (comp-sign (lambda (p) (lambda (x) (p (comp x) (comp pivot)))))
        (less  (filter (comp-sign <=) (cdr l)))
        (great (filter (comp-sign >) (cdr l)))
      )
      
      (append (sort-by comp less) (list pivot) (sort-by comp great))
    )))

;; some getter for our pseudostructure
(define examplePlayers '(("Angel" 14 15) ("Andrei" 8 10) ("Atanas" 10 3) ("Georgi" 6 4)))

(define (getter fieldNumber) 
  (lambda (player) (list-ref player fieldNumber)))

(define get-name (getter 0))
(define get-prediction (getter 1))
(define get-actual (getter 2))

(define (get-error player)
  (abs (- (get-prediction player) (get-actual player))))

;; the actual implementation
(define (finalScores players)
  (letrec 
  (
    ;; sort players by error
    (sorted-players (sort-by get-error players))
    ;; get respective bonuses (reversed proportional)
    (bonuses (reverse (map get-error sorted-players)))
    ;; zip them together, generating the final pairs
    (actual-scores 
      (map 
        (lambda (player bonus) 
          (cons 
            (get-name player) 
            (+ (get-actual player) bonus)))
        sorted-players 
        bonuses))
  ) 
  
  actual-scores))

;; (finalScores examplePlayers) yields the example result :)