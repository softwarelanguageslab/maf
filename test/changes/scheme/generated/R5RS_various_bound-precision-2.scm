; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
(letrec ((f (lambda (x)
              (if (<change> (< x 100) (not (< x 100))) f 1)))
         (f2 (f 5)))
   (eq? (f2 2) f))