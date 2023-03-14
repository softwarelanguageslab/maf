; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((f (lambda (x)
              (if (< x 100) f 1)))
         (f2 (f 5)))
   (<change>
      ()
      (display f2))
   (eq? (f2 2) f))