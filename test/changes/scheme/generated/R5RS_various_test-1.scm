; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((f (lambda (x)
              x))
         (x 1))
   (<change>
      (set! x "hello")
      ((lambda (x) x) (set! x "hello")))
   (<change>
      ()
      "hello")
   (f x))