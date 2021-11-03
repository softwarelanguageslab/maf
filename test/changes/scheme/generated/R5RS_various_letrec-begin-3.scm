; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 0
(letrec ((h (lambda ()
              ()))
         (i 1)
         (res (begin
                (<change>
                   (h)
                   ())
                i)))
   res)