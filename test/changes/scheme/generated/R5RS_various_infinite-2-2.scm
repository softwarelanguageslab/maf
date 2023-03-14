; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((t (lambda (x)
              (<change>
                 ()
                 (+ x 1))
              (t (+ x 1)))))
   (t 0))