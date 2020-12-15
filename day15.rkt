#lang racket/base

(require racket/fixnum
         racket/list
         racket/string)

(define starting-numbers
  (map string->number (string-split (call-with-input-file "input15.txt" read-line) ",")))

(define (interp nums turns)
  (define counts (make-hasheq))
  (for ([(c i) (in-indexed nums)])
    (hash-set! counts c (add1 i)))
  (for/fold ([prev (last nums)])
            ([turn (in-range (length nums) turns)])
    (define new-count
      (let ([t (hash-ref counts prev #f)])
        (if t (fx- turn t) 0)))
    (hash-set! counts prev turn)
    new-count))

(= (interp '(0 3 6) 2020) 436)
(= (interp '(1 3 2) 2020) 1)
(= (interp '(2 1 3) 2020) 10)
(= (interp '(1 2 3) 2020) 27)

(= (interp starting-numbers 2020) 1015)
(= (interp starting-numbers 30000000) 201)
