; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((h (lambda ()
              ()))
         (i 1)
         (res (begin
                (h)
                i)))
   (<change>
      ()
      res)
   (<change>
      ()
      res)
   res)