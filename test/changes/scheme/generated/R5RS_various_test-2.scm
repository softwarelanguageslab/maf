; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((f (lambda (x)
              (<change>
                 x
                 ((lambda (x) x) x))))
         (x 1))
   (<change>
      (set! x "hello")
      ())
   (f x))