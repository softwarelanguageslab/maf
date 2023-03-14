; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((f (lambda (x)
              x))
         (x 1))
   (<change>
      ()
      x)
   (set! x "hello")
   (f x))