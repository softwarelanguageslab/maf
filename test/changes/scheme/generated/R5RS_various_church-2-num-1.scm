; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((zero (lambda (f x)
                 x))
         (inc (lambda (n)
                (<change>
                   (lambda (f x)
                      (f (n f x)))
                   ((lambda (x) x) (lambda (f x) (<change> () (display n)) (f (n f x))))))))
   ((inc (inc zero)) (lambda (x) (+ x 1)) 0))