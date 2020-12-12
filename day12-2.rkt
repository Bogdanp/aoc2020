#lang racket/base

(require racket/match)

(struct instr (cmd n)
  #:transparent)

(struct wp (dir x y)
  #:transparent)

(struct state (wp x y)
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

(define (right-rotate w)
  (match w
    [(wp 'E x y) (wp 'S y (- x))]
    [(wp 'S x y) (wp 'W y (- x))]
    [(wp 'W x y) (wp 'N y (- x))]
    [(wp 'N x y) (wp 'E y (- x))]))

(define (left-rotate w)
  (match w
    [(wp 'E x y) (wp 'N (- y) x)]
    [(wp 'N x y) (wp 'W (- y) x)]
    [(wp 'W x y) (wp 'S (- y) x)]
    [(wp 'S x y) (wp 'E (- y) x)]))

(define (rotate s deg)
  (define steps (quotient deg 90))
  (define w (state-wp s))
  (define-values (rot n)
    (if (negative? steps)
        (values left-rotate  (abs steps))
        (values right-rotate      steps)))
  (define new-w
    (for/fold ([w w])
              ([_ (in-range n)])
      (rot w)))
  (state new-w (state-x s) (state-y s)))

(define (move s dir amt)
  (match-define (state (wp wp-dir wp-x wp-y) ship-x ship-y) s)
  (define-values (new-x new-y)
    (case dir
      [(N) (values wp-x (- wp-y amt))]
      [(S) (values wp-x (+ wp-y amt))]
      [(E) (values (- wp-x amt) wp-y)]
      [(W) (values (+ wp-x amt) wp-y)]))
  (state (wp wp-dir new-x new-y) ship-x ship-y))

(define (follow s amt)
  (match-define (state (and (wp _ wp-x wp-y) w) ship-x ship-y) s)
  (state w
         (+ ship-x (* wp-x amt))
         (+ ship-y (* wp-y amt))))

(define (step s i)
  (match i
    [(instr 'F amt) (follow s    amt)]
    [(instr 'L deg) (rotate s (- deg))]
    [(instr 'R deg) (rotate s    deg)]
    [(instr  d amt) (move   s d  amt)]))

(define (interp s instrs)
  (for/fold ([s s])
            ([i (in-list instrs)])
    (step s i)))

(define (distance s)
  (+ (abs (state-x s))
     (abs (state-y s))))

(define init (state (wp 'E -10 -1) 0 0))
(time (distance (interp init instrs)))
