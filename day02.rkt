#lang racket/base

(require racket/match
         racket/port)

(define lines
  (call-with-input-file "input02.txt" port->lines))

(define spec
  #px"^(\\d+)-(\\d+) (.): (.+)")

(define (read-password-line line)
  (match (regexp-match spec line)
    [(list _
           (app string->number lo)
           (app string->number hi)
           (app (compose1 car string->list) c)
           password)
     (values lo hi c password)]))

(define (password-valid?/old line)
  (define-values (lo hi c password)
    (read-password-line line))
  (define cnt
    (for/sum ([pc (in-list (string->list password))]
              #:when (char=? c pc))
      1))
  (and (>= cnt lo)
       (<= cnt hi)))

(define (password-valid?/new line)
  (define-values (lo hi c password)
    (read-password-line line))
  (define (ref pos)
    (define idx (sub1 pos))
    (and (< idx (string-length password))
         (char=? c (string-ref password idx))))
  (if (ref lo)
      (not (ref hi))
      (ref hi)))

(for/sum ([line (in-list lines)])
  (if (password-valid?/old line) 1 0))

(for/sum ([line (in-list lines)])
  (if (password-valid?/new line) 1 0))
