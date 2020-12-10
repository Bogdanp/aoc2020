#lang racket/base

(require memoize
         racket/list
         racket/set)

(define adapters
  (call-with-input-file "input10.txt"
    (lambda (in)
      (sort
       (for/list ([line (in-lines in)])
         (string->number line))
       <))))

(define part-1
  (for/fold ([ones   0]
             [threes 1]
             [previous 0]
             #:result (* ones threes))
            ([adapter (in-list adapters)])
    (case (- adapter previous)
      [(3) (values ones (add1 threes) adapter)]
      [(1) (values (add1 ones) threes adapter)])))

(define start 0)
(define end (+ 3 (last adapters)))
(define all-adapters (sort (list* start end adapters) <))
(define known-adapters (apply set all-adapters))
(define vertices
  (for*/fold ([vertices (hasheqv)])
             ([adapter (in-list all-adapters)]
              [reach (in-range (- adapter 3) adapter)])
    (if (set-member? known-adapters reach)
        (hash-update vertices reach (λ (vs) (cons adapter vs)) null)
        vertices)))

(define/memo* (subpaths from to)
  (define targets
    (hash-ref vertices from))
  (cond
    [(null? targets) 0]
    [(member to targets) 1]
    [else (for/sum ([t (in-list targets)])
            (subpaths t to))]))

(define arrangements
  (subpaths start end))
