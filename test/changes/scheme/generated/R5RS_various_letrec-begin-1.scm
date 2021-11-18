; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((h (lambda ()
              ()))
         (i 1)
         (res (begin
                (<change>
                   ()
                   h)
                (<change>
                   (h)
                   ((lambda (x) x) (h)))
                i)))
   res)