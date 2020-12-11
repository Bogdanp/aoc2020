#lang racket/base

(define matrix
  (call-with-input-file "input11.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (for/vector ([char (in-string line)])
          char)))))

(define w (vector-length (vector-ref matrix 0)))
(define h (vector-length matrix))

(define (ref m x y)
  (vector-ref (vector-ref m y) x))

(define (floor?    c) (char=? c #\.))
(define (empty?    c) (char=? c #\L))
(define (occupied? c) (char=? c #\#))

(define (count-adjacent m x y)
  (for*/sum ([x* (in-range (max 0 (sub1 x)) (add1 (min (sub1 w) (add1 x))))]
             [y* (in-range (max 0 (sub1 y)) (add1 (min (sub1 h) (add1 y))))]
             #:unless (and (= x x*)
                           (= y y*)))
    (if (occupied? (ref m x* y*)) 1 0)))

(define (count-adjacent-by-sight m x y)
  (define (help x y step-x step-y)
    (define new-x (+ x step-x))
    (define new-y (+ y step-y))
    (cond
      [(or (< new-x 0) (= new-x w)) 0]
      [(or (< new-y 0) (= new-y h)) 0]
      [else
       (define c (ref m new-x new-y))
       (cond
         [(empty?    c) 0]
         [(occupied? c) 1]
         [else (help new-x new-y step-x step-y)])]))
  (+ (help x y  1  0)
     (help x y -1  0)
     (help x y  0  1)
     (help x y  0 -1)
     (help x y  1  1)
     (help x y -1 -1)
     (help x y  1 -1)
     (help x y -1  1)))

(define (step m [ca count-adjacent] [t 4])
  (for/vector #:length h ([y (in-range h)])
    (for/vector #:length w ([x (in-range w)])
      (define c (ref m x y))
      (cond
        [(floor?    c) #\.]
        [(empty?    c) (if (zero? (ca m x y)) #\# c)]
        [(occupied? c) (if (>= (ca m x y) t)  #\L c)]
        [else c]))))

(define (count-occupied m)
  (for*/sum ([x (in-range w)]
             [y (in-range h)])
    (if (occupied? (ref m x y)) 1 0)))

(define (step-until-stable m [ca count-adjacent] [tolerance 4])
  (let loop ([m m])
    (define new-matrix
      (step m ca tolerance))
    (cond
      [(equal? m new-matrix)
       (count-occupied m)]
      [else
       (loop new-matrix)])))

(time (step-until-stable matrix))
(time (step-until-stable matrix count-adjacent-by-sight 5))
