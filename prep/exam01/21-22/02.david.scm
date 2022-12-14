#lang r5rs

(define (map op lst) 
    (if 
        (null? lst) 
        lst 
        (cons (op (car lst)) (map op (cdr lst)))))

(define (foldl op nv l)
  (if (null? l) nv
          (foldl op (op nv (car l)) (cdr l))))

(define (apply-if pred op) 
    (lambda (x) 
        (if (pred x) (op x) x)))

(define (run-machine stack)
    (letrec (
        (consume (lambda (state bin-op n)
            (if 
                (or (< n 1) (< (length state) 2) )
                state
                (let (
                    (first (car state))
                    (second (cadr state))
                    (rest (cddr state)))
                  (if 
                    (or (symbol? first) (symbol? second)) 
                    state
                    (consume 
                        (execute rest (bin-op first second))
                        bin-op
                        (- n 1)))))))
        (execute (lambda (state arg)
            (cond
                ((or (symbol? arg) (number? arg)) 
                    (cons arg state))
                ((procedure? arg) 
                    (map (apply-if number? arg) state))
                ((and (pair? arg) (procedure? (car arg)) (integer? (cdr arg))) 
                    (consume state (car arg) (cdr arg)))
                (else state)
        ))))

        (foldl execute '() stack)))