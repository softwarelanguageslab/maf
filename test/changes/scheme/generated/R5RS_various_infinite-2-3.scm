; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((t (lambda (x)
              (t (+ x 1)))))
   (<change>
      ()
      (display t))
   (<change>
      ()
      (t 0))
   (t 0))