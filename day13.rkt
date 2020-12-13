#lang racket/base

(require math/number-theory
         racket/string)

(define-values (ts ids)
  (call-with-input-file "input13.txt"
    (lambda (in)
      (values
       (string->number (read-line in))
       (map string->number (string-split (read-line in) ","))))))

(define (delta to from)
  (- (+ from (* from (quotient to from))) to))

(apply * (car
          (sort
           (for/list ([id (in-list ids)] #:when id)
             (list id (delta ts id)))
           (lambda (a b)
             (< (cadr a)
                (cadr b))))))

(call-with-values
 (lambda ()
   (for/lists (as ns)
              ([(id idx) (in-indexed ids)]
               #:when id)
     (values (- id idx) id)))
 solve-chinese)
