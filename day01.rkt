#lang racket

(define nums
  (call-with-input-file "input01.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))

(for*/first ([a (in-list nums)]
             [b (in-list nums)]
             #:unless (eqv? a b)
             #:when (= 2020 (+ a b)))
  (* a b))

(for*/first ([a (in-list nums)]
             [b (in-list nums)]
             [c (in-list nums)]
             #:unless (eqv? a b)
             #:unless (eqv? b c)
             #:when (= 2020 (+ a b c)))
  (* a b c))


;; Generalized ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (find-solution stx)
  (syntax-case stx ()
    [(_ nums ns)
     (let ([make-ids (lambda (p)
                       (for/list ([i (in-range (syntax->datum #'ns))])
                         (string->symbol (format "~a~a" p i))))])
       (with-syntax ([(n-id ...) (make-ids "n-")]
                     [(p-id ...) (make-ids "p-")])
         #'(let ([xs nums])
             (for*/first ([(n-id p-id) (in-indexed xs)] ...
                          #:when (= ns (length (remove-duplicates (list p-id ...))))
                          #:when (= 2020 (+ n-id ...)))
               (* n-id ...)))))]))

(find-solution nums 1)
(find-solution nums 2)
(find-solution nums 3)
(find-solution nums 4)
