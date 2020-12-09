#lang racket/base

(require racket/list
         racket/vector)

(define numbers
  (call-with-input-file "input09.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (string->number line)))))

(define (valid? preamble n)
  (for*/first ([(x i) (in-indexed preamble)]
               [(y j) (in-indexed preamble)]
               #:unless (= i j)
               #:when (= n (+ x y)))
    #t))

(define (find-invalid-number ns [preamble-len 25])
  (for/first ([(n i) (in-indexed ns)]
              #:when (>= i preamble-len)
              #:unless (valid? (vector-copy ns (- i preamble-len) i) n))
    n))

(define invalid-number
  (find-invalid-number numbers))

(define (find-contiguous-run ns n)
  (let loop ([idx 0]
             [subidx 0]
             [run null]
             [longest-run null])
    (define pos (+ idx subidx))
    (cond
      [(< pos (vector-length ns))
       (define new-run (cons (vector-ref ns pos) run))
       (define run-sum (apply + new-run))
       (cond
         [(= run-sum n)
          (loop (add1 idx) 0 null (if (> (length new-run)
                                         (length longest-run))
                                      new-run
                                      longest-run))]
         [(< run-sum n)
          (loop idx (add1 subidx) new-run longest-run)]

         [else
          (loop (add1 idx) 0 null longest-run)])]
      [else
       (sort longest-run <)])))

(define run
  (find-contiguous-run numbers invalid-number))

(define weakness
  (+ (first run)
     (last  run)))
