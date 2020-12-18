#lang racket/base

(require racket/list
         racket/match
         racket/port)

(define (calc line [tbl (hasheq + 1 * 1)])
  (define e
    (read (open-input-string (format "(~a)" line))))
  (define (prec op)
    (hash-ref tbl op))
  (let loop ([e e] [nums null] [ops  null])
    (define (push-num e* num)
      (loop e* (cons num nums) ops))
    (define (push-op e* op)
      (cond
        [(null? ops) (loop e* nums (cons op ops))]
        [(< (prec (car ops)) (prec op)) (loop e* nums (cons op ops))]
        [else (loop e* (cons ((car ops) (cadr nums) (car nums)) (cddr nums)) (cons op (cdr ops)))]))
    (match e
      [(? null?)
       (for/fold ([nums nums] #:result (car nums))
                 ([op (in-list ops)])
         (cons (op (cadr nums) (car nums)) (cddr nums)))]
      [(cons (? list? sub-e) e*)
       (push-num e* (loop sub-e null null))]
      [(cons '+ e*) (push-op  e* +)]
      [(cons '* e*) (push-op  e* *)]
      [(cons  n e*) (push-num e* n)])))

(define lines
  (call-with-input-file "input18.txt" port->lines))

(=
 (apply + (for/list ([line (in-list lines)])
            (calc line)))
 11297104473091)

(=
 (apply + (let ([tbl (hasheq + 2 * 1)])
            (for/list ([line (in-list lines)])
              (calc line tbl))))
 185348874183674)
