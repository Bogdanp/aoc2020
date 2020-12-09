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
  (define-values (lo hi)
    (let loop ([lo 0]
               [hi (vector-length ns)])
      (define sum
        (for/sum ([n (in-vector ns lo hi)])
          n))
      (cond
        [(> sum n) (loop lo (sub1 hi))]
        [(< sum n) (loop (add1 lo) (add1 hi))]
        [else (values lo hi)])))

  (for/list ([n (in-vector ns lo hi)])
    n))

(define run
  (find-contiguous-run numbers invalid-number))

(define weakness
  (+ (apply min run)
     (apply max run)))
