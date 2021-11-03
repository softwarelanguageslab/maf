; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((t (lambda (x)
              (<change>
                 ()
                 (display t))
              (t (+ x 1)))))
   (t 0))