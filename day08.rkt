#lang racket/base

(require racket/match
         racket/set
         racket/vector)

(struct instr (opcode arg)
  #:transparent)

(define (parse-instr line)
  (match (regexp-match #rx"^(acc|jmp|nop) (.+)$" line)
    [(list _
           (app string->symbol op)
           (app string->number arg))
     (instr op arg)]))

(define instrs
  (call-with-input-file "input08.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (parse-instr line)))))

(define (step instrs pc acc)
  (match (vector-ref instrs pc)
    [(instr 'nop _)   (values (add1 pc) acc)]
    [(instr 'acc arg) (values (add1 pc) (+ acc arg))]
    [(instr 'jmp arg) (values (+ pc arg) acc)]))

(define (interp instrs)
  (let loop ([pc 0]
             [acc 0]
             [seen (set)])
    (define-values (new-pc new-acc)
      (step instrs pc acc))
    (cond
      [(set-member? seen new-pc)
       (values 'break new-acc)]
      [(< new-pc (vector-length instrs))
       (loop new-pc new-acc (set-add seen pc))]
      [else
       (values 'end acc)])))

(define (patch-instrs instrs pc)
  (define new-instrs (vector-copy instrs))
  (define new-instr
    (match (vector-ref new-instrs pc)
      [(instr 'nop arg) (instr 'jmp arg)]
      [(instr 'jmp arg) (instr 'nop arg)]))
  (begin0 new-instrs
    (vector-set! new-instrs pc new-instr)))

(define (fix-instrs/brute-force instrs)
  (define poss-to-patch
    (for/list ([(i pc) (in-indexed instrs)]
               #:when (or (eq? (instr-opcode i) 'nop)
                          (eq? (instr-opcode i) 'jmp)))
      pc))
  (for*/or ([pc (in-list poss-to-patch)]
            [patched-instrs (in-value (patch-instrs instrs pc))])
    (define-values (exit acc)
      (interp patched-instrs))
    (and (eq? exit 'end) acc)))

(define-values (_ broken-acc)
  (interp instrs))

(define patched-acc
  (fix-instrs/brute-force instrs))
