; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((f (lambda (x)
              (<change>
                 ()
                 (display f))
              (if (< x 100) f 1)))
         (f2 (f 5)))
   (eq? (f2 2) f))