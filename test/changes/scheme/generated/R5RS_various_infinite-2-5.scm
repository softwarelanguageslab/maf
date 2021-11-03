; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((t (lambda (x)
              (t (+ x 1)))))
   (<change>
      ()
      t)
   (t 0))