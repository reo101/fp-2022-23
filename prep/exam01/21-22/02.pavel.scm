#lang r5rs

(define head car)
(define tail cdr)
(define :    cons)

(define map
  (lambda (f l)
    (if (null? l)
      l
      (cons (f (head l))
            (map f (tail l))))))

(define run-machine
  (lambda (ops)
    (define run-machine-stack
      (lambda (ops stack)
        (if (null? ops)
          stack
          (let
            ((curr-op (head ops))
             (rest-op (tail ops)))
            (cond
              ((or (number? curr-op)
                   (symbol? curr-op))
               (run-machine-stack
                 rest-op
                 (: curr-op stack)))
              ((procedure? curr-op)
               (run-machine-stack
                 rest-op
                 (map (lambda (elem)
                        (if (number? elem)
                          (curr-op elem)
                          elem))
                      stack)))
              ((and (pair?      curr-op)
                    (procedure? (head curr-op))
                    (integer?   (tail curr-op)))
               (let ((curr-op (head curr-op))
                     (count   (tail curr-op)))
                 (if (or (< (length stack) 2)
                         (= count 0)
                         (or (symbol? (car  stack))
                             (symbol? (cadr stack))))
                   (run-machine-stack
                     rest-op
                     stack)
                   (let ((fst  (car  stack))
                         (snd  (cadr stack))
                         (rest (cddr stack)))
                     (run-machine-stack
                       (: (: curr-op (- count 1))
                          rest-op)
                       (: (curr-op fst snd)
                          rest))))))
              (else
               (run-machine-stack
                 rest-op
                 stack)))))))
    (run-machine-stack ops '())))

;; (run-machine (list 1 'x 4 'a 9 16 25 sqrt 6))                       → (6 5 4 3 a 2 x 1)
;; (run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5))) → (45 a 2 x 1)
