; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((f (lambda (x)
              (if (<change> (< x 100) (not (< x 100))) f 1)))
         (f2 (f 5)))
   (eq? (f2 2) f))