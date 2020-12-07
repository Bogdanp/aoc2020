#lang racket/base

(require racket/hash
         racket/match
         racket/string)

(struct vertex (from to n)
  #:transparent)

(define (parse-edges from tos)
  (for/list ([to-str (in-list (string-split tos ", "))])
    (match to-str
      [(regexp #px"(\\d+) (.+) bags?.?" (list _ (app string->number n) to))
       (vertex from to n)])))

(define vertices
  (call-with-input-file "input07.txt"
    (lambda (in)
      (for/fold ([vertices null])
                ([line (in-lines in)])
        (match line
          [(regexp #rx"^(.+) bags contain no other bags.") vertices]
          [(regexp #rx"^(.+) bags contain (.+)" (list _ who what))
           (append vertices (parse-edges who what))])))))

(define (holders-of to)
  (for/fold ([holders (hash)])
            ([v (in-list vertices)])
    (match v
      [(vertex from (== to) _)
       (hash-union
        (hash-set holders from #t)
        (holders-of from)
        #:combine (λ (a _) a))]
      [_ holders])))

(define total-holders
  (hash-count (holders-of "shiny gold")))

(define (bags-held from)
  (for/sum ([v (in-list vertices)])
    (match v
      [(vertex (== from) to n)
       (+ n (* n (bags-held to)))]
      [_ 0])))

(define total-bags-held
  (bags-held "shiny gold"))
