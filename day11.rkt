#lang racket/base

(require racket/match
         racket/port)

(define matrix
  (call-with-input-file "input11.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (for/vector ([char (in-string line)])
          char)))))

(define w (vector-length (vector-ref matrix 0)))
(define h (vector-length matrix))

(define (pos x y)
  (vector-ref (vector-ref matrix y) x))

(define (floor?    x y) (char=? (pos x y) #\.))
(define (empty?    x y) (char=? (pos x y) #\L))
(define (occupied? x y) (char=? (pos x y) #\#))

(define (count-adjacent x y)
  (for*/sum ([x* (in-range (max 0 (sub1 x)) (add1 (min (sub1 w) (add1 x))))]
             [y* (in-range (max 0 (sub1 y)) (add1 (min (sub1 h) (add1 y))))]
             #:unless (and (= x x*)
                           (= y y*)))
    (if (occupied? x* y*) 1 0)))

(define (count-adjacent-by-sight x y)
  (define (help x y step-x step-y)
    (define new-x (+ x step-x))
    (define new-y (+ y step-y))
    (cond
      [(or (< new-x 0) (= new-x w)) 0]
      [(or (< new-y 0) (= new-y h)) 0]
      [(empty?    new-x new-y) 0]
      [(occupied? new-x new-y) 1]
      [else (help new-x new-y step-x step-y)]))
  (+ (help x y  1  0)
     (help x y -1  0)
     (help x y  0  1)
     (help x y  0 -1)
     (help x y  1  1)
     (help x y -1 -1)
     (help x y  1 -1)
     (help x y -1  1)))

(define (leave!  x y) (vector-set! (vector-ref matrix y) x #\L))
(define (occupy! x y) (vector-set! (vector-ref matrix y) x #\#))

(define (step [ca count-adjacent] [tolerance 4])
  (define instructions
    (for*/list ([x (in-range w)]
                [y (in-range h)]
                #:unless (floor? x y))
      (cond
        [(empty? x y)
         (if (zero? (ca x y))
             (list 'occupy x y)
             (list 'noop   x y))]
        [(occupied? x y)
         (if (>= (ca x y) tolerance)
             (list 'leave x y)
             (list 'noop  x y))]
        [else
         (list 'noop x y)])))
  (for ([i (in-list instructions)])
    (match i
      [(list 'occupy x y) (occupy! x y)]
      [(list 'leave  x y) (leave!  x y)]
      [_ (void)]))
  (matrix->string))

(define (matrix->string)
  (call-with-output-string
   (lambda (out)
     (for ([(row i) (in-indexed matrix)])
       (when (> i 0)
         (newline out))
       (for ([col (in-vector row)])
         (display col out))))))

(define (occupied-count)
  (for*/sum ([x (in-range w)]
             [y (in-range h)])
    (if (occupied? x y) 1 0)))

(define (step-until-stable [ca count-adjacent] [tolerance 4])
  (let loop ([seen (list (matrix->string))])
    (define new-matrix
      (step ca tolerance))
    (cond
      [(member new-matrix seen)
       (occupied-count)]
      [else
       (loop (cons new-matrix seen))])))

#;(step-until-stable)
#;(step-until-stable count-adjacent-by-sight 5)
