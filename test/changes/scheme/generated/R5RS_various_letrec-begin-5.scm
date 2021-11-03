; Changes:
; * removed: 0
; * added: 1
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
      (display res))
   res)