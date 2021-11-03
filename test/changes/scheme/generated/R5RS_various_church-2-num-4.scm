; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((zero (lambda (f x)
                 x))
         (inc (lambda (n)
                (lambda (f x)
                   (f (n f x))))))
   (<change>
      ()
      x)
   (<change>
      ()
      zero)
   ((inc (inc zero)) (lambda (x) (+ x 1)) 0))