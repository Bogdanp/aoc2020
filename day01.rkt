#lang racket/base

(define nums
  (call-with-input-file "input1.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))

(for*/first ([a (in-list nums)]
             [b (in-list nums)]
             #:unless (eqv? a b)
             #:when (= 2020 (+ a b)))
  (* a b))

(for*/first ([a (in-list nums)]
             [b (in-list nums)]
             [c (in-list nums)]
             #:unless (eqv? a b)
             #:unless (eqv? b c)
             #:when (= 2020 (+ a b c)))
  (* a b c))
