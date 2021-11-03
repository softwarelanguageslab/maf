; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((f (lambda ()
              (<change>
                 ()
                 f)
              (f))))
   (f))