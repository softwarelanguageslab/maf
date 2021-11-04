; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((h (lambda ()
              (<change>
                 ()
                 ((lambda (x) x) ()))))
         (i 1)
         (res (begin
                (<change>
                   (h)
                   ())
                i)))
   (<change>
      res
      ((lambda (x) x) res)))