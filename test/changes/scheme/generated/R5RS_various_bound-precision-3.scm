; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((f (lambda (x)
              (<change>
                 ()
                 x)
              (if (< x 100) f 1)))
         (f2 (f 5)))
   (eq? (f2 2) f))