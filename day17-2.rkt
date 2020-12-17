#lang racket/base

(require racket/match
         racket/port)

(struct pos (x y z w) #:transparent)

(define (pos->list p)
  (list (pos-x p)
        (pos-y p)
        (pos-z p)
        (pos-w p)))

(define cubes
  (call-with-input-file "input17.txt"
    (lambda (in)
      (for*/hash ([(l y) (in-indexed (port->lines in))]
                  [(c x) (in-indexed l)])
        (values (pos x y 0 0) (char=? c #\#))))))

(define (adjacent-positions p)
  (match-define (pos x y z w) p)
  (for*/list ([x* (in-range (sub1 x) (+ x 2))]
              [y* (in-range (sub1 y) (+ y 2))]
              [z* (in-range (sub1 z) (+ z 2))]
              [w* (in-range (sub1 w) (+ w 2))]
              [p* (in-value (pos x* y* z* w*))]
              #:unless (equal? p* p))
    p*))

(define (active-neighbors cs p)
  (for/sum ([p* (in-list (adjacent-positions p))]
            #:when (hash-ref cs p* #f))
    1))

(define (find-bound cs)
  (apply max (for*/list ([p (hash-keys cs)]
                         [v (in-list (pos->list p))])
               (abs v))))

(define (step cs)
  (define bound (find-bound cs))
  (for*/fold ([cs* cs])
             ([x (in-range (- bound) (+ bound 2))]
              [y (in-range (- bound) (+ bound 2))]
              [z (in-range (- bound) (+ bound 2))]
              [w (in-range (- bound) (+ bound 2))]
              [p (in-value (pos x y z w))])
    (define active? (hash-ref cs p #f))
    (define inactive? (not active?))
    (define n (active-neighbors cs p))
    (if (or (and active?   (or (= n 2) (= n 3)))
            (and inactive? (= n 3)))
        (hash-set cs* p #t)
        (hash-remove cs* p))))

(define (interp cs steps)
  (for/fold ([cs cs])
            ([_ (in-range steps)])
    (step cs)))

(define (display-cubes cs [z 0] [w 0])
  (define bound (find-bound cs))
  (for ([y (in-range (- bound) (add1 bound))])
    (newline)
    (for* ([x (in-range (- bound) (add1 bound))]
           [p (in-value (pos x y z w))])
      (begin0 y
        (if (hash-ref cs p #f)
            (display #\#)
            (display #\.))))))

(= (hash-count (interp cubes 6)) 2472)
