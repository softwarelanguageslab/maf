; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((f (lambda ()
              (<change>
                 (f)
                 ((lambda (x) x) (f))))))
   (<change>
      ()
      f)
   (f))