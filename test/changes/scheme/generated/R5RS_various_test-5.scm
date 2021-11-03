; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
(letrec ((f (lambda (x)
              x))
         (x 1))
   (<change>
      (set! x "hello")
      (f x))
   (<change>
      (f x)
      (set! x "hello")))