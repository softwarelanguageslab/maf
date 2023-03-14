; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((h (lambda ()
              (<change>
                 ()
                 ((lambda (x) x) ()))))
         (i 1)
         (res (begin
                (<change>
                   (h)
                   i)
                (<change>
                   i
                   (h)))))
   res)