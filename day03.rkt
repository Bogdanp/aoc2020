#lang racket/base

(struct worldmap (w h m)
  #:transparent)

(define m
  (call-with-input-file "input03.txt"
    (lambda (in)
      (define m
        (for/vector ([line (in-lines in)])
          (for/vector ([c (in-string line)])
            (case c
              [(#\.) #f]
              [(#\#) #t]))))
      (worldmap (vector-length (vector-ref m 0))
                (vector-length m)
                m))))

(define (step m x y r d)
  (define new-x (remainder (+ x r) (worldmap-w m)))
  (define new-y (remainder (+ y d) (worldmap-h m)))
  (define row (vector-ref (worldmap-m m) new-y))
  (define col (vector-ref row new-x))
  (values new-x new-y col))

(define (check m r d)
  (let loop ([x 0]
             [y 0]
             [hits 0])
    (cond
      [(= y (sub1 (worldmap-h m))) hits]
      [else
       (define-values (new-x new-y tree?)
         (step m x y r d))
       (loop new-x new-y (if tree? (add1 hits) hits))])))

(check m 3 1)

(* (check m 1 1)
   (check m 3 1)
   (check m 5 1)
   (check m 7 1)
   (check m 1 2))
