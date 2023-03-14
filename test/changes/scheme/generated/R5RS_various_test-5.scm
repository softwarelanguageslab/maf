; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((f (lambda (x)
              (<change>
                 ()
                 x)
              x))
         (x 1))
   (<change>
      (set! x "hello")
      (f x))
   (<change>
      (f x)
      (set! x "hello")))