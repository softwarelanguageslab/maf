; Changes:
; * removed: 1
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((f (lambda (x)
              x))
         (x 1))
   (<change>
      ()
      f)
   (<change>
      (set! x "hello")
      ())
   (f x))