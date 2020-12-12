#lang racket/base

(require racket/match)

(struct instr (cmd n)
  #:transparent)

(struct state (dir x y)
  #:transparent)

(define instr-re #rx"([NSEWLRF])(.+)")
(define instrs
  (call-with-input-file "input12.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (match (regexp-match instr-re line)
          [(list _
                 (app string->symbol cmd)
                 (app string->number n))
           (instr cmd n)])))))

(define (forward s amt)
  (match s
    [(state 'N x y) (state 'N x (- y amt))]
    [(state 'S x y) (state 'S x (+ y amt))]
    [(state 'E x y) (state 'E (- x amt) y)]
    [(state 'W x y) (state 'W (+ x amt) y)]))

(define (directions dir)
  (case dir
    [(N) '(E S W N)]
    [(W) '(N E S W)]
    [(E) '(S W N E)]
    [(S) '(W N E S)]))

(define (reverse-directions dir)
  (case dir
    [(N) '(W S E N)]
    [(W) '(S E N W)]
    [(S) '(E N W S)]
    [(E) '(N W S E)]))

(define (rotate s deg)
  (define steps (quotient deg 90))
  (define dir (state-dir s))
  (define-values (n dirs)
    (if (negative? steps)
        (values (abs steps) (reverse-directions dir))
        (values steps (directions dir))))
  (define new-dir
    (for/fold ([dir (state-dir s)])
              ([_ (in-range n)]
               [d (in-cycle dirs)])
      d))
  (state new-dir (state-x s) (state-y s)))

(define (move s dir amt)
  (match (forward (state dir (state-x s) (state-y s)) amt)
    [(state _ x y) (state (state-dir s) x y)]))

(define (step s i)
  (match i
    [(instr 'F amt) (forward s    amt)]
    [(instr 'L deg) (rotate  s (- deg))]
    [(instr 'R deg) (rotate  s    deg)]
    [(instr  d amt) (move    s d  amt)]))

(define (interp s instrs)
  (for/fold ([s s])
            ([i (in-list instrs)])
    (step s i)))

(define (distance s)
  (+ (abs (state-x s))
     (abs (state-y s))))

(time (distance (interp (state 'E 0 0) instrs)))
