; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((zero (lambda (f x)
                 x))
         (inc (lambda (n)
                (lambda (f x)
                   (f (n f x))))))
   ((inc (inc zero)) (lambda (x) (<change> () +) (+ x 1)) 0))