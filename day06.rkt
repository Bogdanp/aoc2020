#lang racket/base

(struct group (members answers)
  #:transparent)

(define groups
  (call-with-input-file "input06.txt"
    (lambda (in)
      (for/fold ([groups null]
                 [members 0]
                 [answers (hasheq)]
                 #:result (reverse (cons (group members answers) groups)))
                ([line (in-lines in)])
        (cond
          [(string=? line "")
           (values (cons (group members answers) groups) 0 (hasheq))]
          [else
           (values groups (add1 members) (for/fold ([answers answers]
                                                    [seen? (hasheq)]
                                                    #:result answers)
                                                   ([c (in-string line)])
                                           (cond
                                             [(hash-has-key? seen? c)
                                              (values answers seen?)]
                                             [else
                                              (values
                                               (hash-update answers c add1 0)
                                               (hash-set seen? c #t))])))])))))

(define total-answered-questions
  (apply + (map (compose1 hash-count group-answers) groups)))

(define all-yesses
  (for*/sum ([g (in-list groups)]
             [answers (in-hash-values (group-answers g))])
    (if (= (group-members g) answers) 1 0)))
