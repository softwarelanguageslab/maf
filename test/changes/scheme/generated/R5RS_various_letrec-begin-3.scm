; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((h (lambda ()
              ()))
         (i 1)
         (res (begin
                (h)
                (<change>
                   ()
                   (display i))
                i)))
   res)