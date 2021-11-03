; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((f (lambda (x)
              x))
         (x 1))
   (<change>
      ()
      (f x))
   (set! x "hello")
   (f x))