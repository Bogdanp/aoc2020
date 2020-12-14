#lang racket/base

(require (for-syntax racket/base)
         racket/list
         racket/match)

(define mask-re #px"mask = (.{36})")
(define mem-re  #px"mem\\[(.+)\\] = (.+)")

(struct assignment (addr val) #:transparent)
(struct group (mask assignments) #:transparent)

(define-match-expander mask
  (syntax-rules ()
    [(_ id)
     (regexp mask-re (list _ id))]))

(define-match-expander instr
  (syntax-rules ()
    [(_ addr-id val-id)
     (regexp mem-re (list _
                          (app string->number addr-id)
                          (app string->number val-id)))]))

(define groups
  (call-with-input-file "input14.txt"
    (lambda (in)
      (match-define (mask m)
        (read-line in))
      (for/fold ([m m]
                 [as null]
                 [gs null]
                 #:result (reverse (cons (group m (reverse as)) gs)))
                ([line (in-lines in)])
        (match line
          [(mask new-m)
           (values new-m null (cons (group m (reverse as)) gs))]

          [(instr addr val)
           (values m (cons (assignment addr val) as) gs)])))))

(define (mask-apply m n)
  (for*/fold ([n n]
              [s 0]
              #:result (bitwise-and n (sub1 (expt 2 36))))
             ([i (in-range (sub1 (string-length m)) -1 -1)]
              [c (in-value (string-ref m i))])
    (case c
      [(#\X) (values n (add1 s))]
      [(#\0) (values (bitwise-and n (bitwise-not (arithmetic-shift 1 s))) (add1 s))]
      [(#\1) (values (bitwise-ior n (arithmetic-shift 1 s)) (add1 s))])))

(define part-1
  (for*/fold ([mem (hash)] #:result (apply + (hash-values mem)))
             ([g (in-list groups)]
              [m (in-value (group-mask g))]
              [a (in-list (group-assignments g))])
    (hash-set mem
              (assignment-addr a)
              (mask-apply m (assignment-val a)))))

(= part-1 5875750429995)

(define all-zero
  (make-list 36 #\0))

(define (pre-mask m n)
  (for/fold ([chars null] #:result (apply string chars))
            ([c1 (in-list  (reverse (string->list m)))]
             [c2 (in-cycle (reverse (string->list (number->string n 2))) all-zero)])
    (case c1
      [(#\X) (cons #\X chars)]
      [(#\0) (cons c2  chars)]
      [(#\1) (cons c1  chars)])))

(define (mask->addrs m)
  (let loop ([cs (reverse (string->list m))] [n 0] [s 0])
    (match cs
      [(? null?) (list n)]
      [(cons #\0 cs) (loop cs n (add1 s))]
      [(cons #\1 cs) (loop cs (bitwise-ior n (arithmetic-shift 1 s)) (add1 s))]
      [(cons #\X cs) (append
                      (loop cs n (add1 s))
                      (loop cs (bitwise-ior n (arithmetic-shift 1 s)) (add1 s)))])))

(define part-2
  (for*/fold ([mem (hash)] #:result (apply + (hash-values mem)))
             ([g (in-list groups)]
              [m (in-value (group-mask g))]
              [a (in-list (group-assignments g))]
              [l (in-list (mask->addrs (pre-mask m (assignment-addr a))))])
    (hash-set mem l (assignment-val a))))

(= part-2 5272149590143)
