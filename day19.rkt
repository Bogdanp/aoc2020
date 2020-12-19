#lang racket/base

(require brag/support
         racket/file
         racket/list
         racket/match
         racket/port
         racket/sandbox
         racket/string
         threading)

(struct lit (c) #:transparent)
(struct seq (ids) #:transparent)
(struct alt (a b) #:transparent)

(define (parse-rule s)
  (match-define
    (pregexp #px"(.+): (.+)" (list _ (app string->number id) re))
    s)
  (values id (let parse-re ([re re])
               (match re
                 ["\"a\"" (lit #\a)]
                 ["\"b\"" (lit #\b)]
                 [(regexp "(.+) \\| (.+)" (list _ lhs rhs))
                  (alt (parse-re lhs)
                       (parse-re rhs))]
                 [_
                  (seq (map string->number
                            (string-split re " ")))]))))

(define (rules->grammar tbl)
  (define (display-id id)
    (printf "r~a" id))
  (with-output-to-string
    (lambda ()
      (displayln "#lang brag")
      (for ([(id r) (in-hash tbl)])
        (match r
          [(lit c)
           (display-id id)
           (display ": ")
           (display #\")
           (display (case c
                      [(#\a) "A"]
                      [(#\b) "B"]))
           (display #\")
           (newline)]
          [(seq ids)
           (display-id id)
           (display ": ")
           (for ([id (in-list ids)])
             (display-id id)
             (display #\space))
           (newline)]
          [(alt a b)
           (display-id id)
           (display ": ")
           (for ([id (in-list (seq-ids a))])
             (display-id id)
             (display #\space))
           (display "| ")
           (for ([id (in-list (seq-ids b))])
             (display-id id)
             (display #\space))
           (newline)])))))

(define-values (rules messages)
  (call-with-input-file "input19.txt"
    (lambda (in)
      (for/fold ([state 'rules]
                 [rules (hasheqv)]
                 [messages null]
                 #:result (values rules (reverse messages)))
                ([line (in-lines in)])
        (match* (state line)
          [(_ "")
           (values 'messages rules null)]
          [('rules s)
           (let-values ([(id r) (parse-rule s)])
             (values 'rules (hash-set rules id r) null))]
          [('messages m)
           (values 'messages rules (cons m messages))])))))

(define (rules->parser rs)
  (define path (make-temporary-file))
  (call-with-output-file path
    #:exists 'truncate/replace
    (lambda (out)
      (displayln (rules->grammar rs) out)))

  (dynamic-require path 'parse))

(define (tokenize m)
  (for/list ([c (in-string m)])
    (case c
      [(#\a) (token 'A "a")]
      [(#\b) (token 'B "b")])))

(define (solution p ms)
  (for/sum ([m (in-list ms)])
    (with-handlers ([exn:fail? (lambda (_) 0)])
      (begin0 1
        (p (tokenize m))))))

(= (solution (rules->parser rules) messages) 132)

(define (add-rule h s)
  (define-values (id r)
    (parse-rule s))
  (hash-set h id r))

(define updated-rules
  (~> rules
      (add-rule "8: 42 | 42 8")
      (add-rule "11: 42 31 | 42 11 31")))

(= (solution (rules->parser updated-rules) messages) 306)
