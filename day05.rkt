#lang racket/base

(require racket/match)

(define (fx/ a b)
  (inexact->exact (round (/ a b))))

(define (lower-half lo hi)
  (values lo (- hi (fx/ (- hi lo) 2))))

(define (upper-half lo hi)
  (values (+ lo (fx/ (- hi lo) 2)) hi))

(define (parse-seat l)
  (define in (open-input-string l))
  (let loop ([row    0]
             [row-lo 0]
             [row-hi 127]
             [col    0]
             [col-lo 0]
             [col-hi 7])
    (case (read-char in)
      [(#\F) (let-values ([(new-row-lo new-row-hi) (lower-half row-lo row-hi)])
               (loop new-row-lo new-row-lo new-row-hi col col-lo col-hi))]
      [(#\B) (let-values ([(new-row-lo new-row-hi) (upper-half row-lo row-hi)])
               (loop new-row-hi new-row-lo new-row-hi col col-lo col-hi))]
      [(#\L) (let-values ([(new-col-lo new-col-hi) (lower-half col-lo col-hi)])
               (loop row row-lo row-hi new-col-lo new-col-lo new-col-hi))]
      [(#\R) (let-values ([(new-col-lo new-col-hi) (upper-half col-lo col-hi)])
               (loop row row-lo row-hi new-col-hi new-col-lo new-col-hi))]
      [else (cons row col)])))

(define seats
  (call-with-input-file "input05.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (parse-seat line)))))

(define seat-ids
  (sort
   (for/list ([seat (in-list seats)])
     (match-define (cons row col) seat)
     (+ (* row 8) col))
   <))

(define highest-id
  (apply max seat-ids))

(define missing-id
  (for/fold ([last-id (car seat-ids)] #:result (add1 last-id))
            ([seat-id (in-list (cdr seat-ids))])
    #:break (> (- seat-id last-id) 1)
    seat-id))
