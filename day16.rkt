#lang racket/base

(require racket/match
         racket/string)

(struct range (lo hi) #:transparent)
(struct rule (kind ranges) #:transparent)

(define (parse-rule line)
  (match line
    [(pregexp #px"([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)"
              (list _
                    (app string->symbol kind)
                    (app string->number lo1)
                    (app string->number hi1)
                    (app string->number lo2)
                    (app string->number hi2)))
     (rule kind (list (range lo1 hi1)
                      (range lo2 hi2)))]))

(define (parse-ticket line)
  (list->vector (map string->number (string-split line ","))))

(define-values (rules my-ticket nearby-tickets)
  (call-with-input-file "input16.txt"
    (lambda (in)
      (for/fold ([state 'parse-rules]
                 [rules null]
                 [my-ticket #f]
                 [nearby-tickets null]
                 #:result (values (reverse rules) my-ticket (reverse nearby-tickets)))
                ([line (in-lines in)])
        (match* (state line)
          [(_ "") (values state rules my-ticket nearby-tickets)]
          [(_ "your ticket:") (values 'parse-mine rules #f null)]
          [(_ "nearby tickets:") (values 'parse-nearby rules my-ticket null)]
          [('parse-rules _) (values state (cons (parse-rule line) rules) #f null)]
          [('parse-mine _) (values state rules (parse-ticket line) null)]
          [('parse-nearby _) (values state rules my-ticket (cons (parse-ticket line) nearby-tickets))])))))

(define (valid? r v)
  (for/or ([rng (in-list (rule-ranges r))])
    (and (>= v (range-lo rng))
         (<= v (range-hi rng)))))

(define (invalid-values-for-any-rule rs t)
  (for*/list ([v (in-vector t)]
              [ok? (in-value
                    (for/or ([r (in-list rs)])
                      (valid? r v)))]
              #:unless ok?)
    v))

(define (find-scanning-error-rate r ts)
  (for*/sum ([t (in-list ts)]
             [v (in-list (invalid-values-for-any-rule r t))])
    v))

(define valid-nearby-tickets
  (for*/list ([ts (in-list nearby-tickets)]
              [ok? (in-value (null? (invalid-values-for-any-rule rules ts)))]
              #:when ok?)
    ts))

(define (tickets-possible-fields rs t)
  (for/list ([v (in-vector t)])
    (for/list ([r (in-list rs)] #:when (valid? r v))
      (rule-kind r))))

(define (find-field-positions rs ts)
  (define n (length ts))
  (define counts-by-field&pos
    (for*/fold ([counts (hash)])
               ([t (in-list ts)]
                [(fs pos) (in-indexed (tickets-possible-fields rs t))]
                [f (in-list fs)])
      (hash-update counts f
                   (lambda (c)
                     (hash-update c pos add1 0))
                   hash)))
  (define (fields-used-once counts [taken-positions null])
    (for*/list ([(f poss) (in-hash counts)]
                [positions (in-value
                            (for/list ([(p cnt) (in-hash poss)]
                                       #:unless (member p taken-positions)
                                       #:when (= cnt n))
                              p))]
                #:when (= (length positions) 1))
      (cons f (car positions))))
  (let loop ([candidates counts-by-field&pos]
             [positions (hasheqv)])
    (cond
      [(hash-empty? candidates) positions]
      [else
       (define-values (new-candidates new-positions)
         (for/fold ([candidates candidates]
                    [positions positions])
                   ([field&pos (in-list (fields-used-once candidates (hash-keys positions)))])
           (match-define (cons f pos) field&pos)
           (values (hash-remove candidates f)
                   (hash-set positions pos f))))
       (loop new-candidates new-positions)])))

(define field-positions
  (find-field-positions rules (cons my-ticket valid-nearby-tickets)))

(= (find-scanning-error-rate rules nearby-tickets) 28873)

(=
 (for*/product ([(pos f) (in-hash field-positions)]
                #:when (string-prefix? (symbol->string f) "departure")
                [v (in-value (vector-ref my-ticket pos))])
   v)
 2587271823407)
