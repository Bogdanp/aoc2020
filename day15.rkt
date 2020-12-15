#lang racket/base

(require racket/string)

(define starting-numbers
  (map string->number (string-split (call-with-input-file "input15.txt" read-line) ",")))

(define (interp nums turns)
  (let loop ([starting nums]
             [counts (hasheqv)]
             [last #f]
             [turn 0])
    (cond
      [(= turn turns) last]
      [(null? starting)
       (loop starting
             (hash-set counts last turn)
             (cond
               [(hash-ref counts last #f) => (λ (t) (- turn t))]
               [else 0])
             (add1 turn))]
      [else
       (loop (cdr starting)
             (hash-set counts last turn)
             (car starting)
             (add1 turn))])))

(= (interp '(0 3 6) 2020) 436)
(= (interp '(1 3 2) 2020) 1)
(= (interp '(2 1 3) 2020) 10)
(= (interp '(1 2 3) 2020) 27)

(= (interp starting-numbers 2020) 1015)
(= (interp starting-numbers 30000000) 201)
