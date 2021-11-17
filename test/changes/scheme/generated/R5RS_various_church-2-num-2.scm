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
                (lambda (f x)
                   (<change>
                      (f (n f x))
                      ((lambda (x) x) (f (n f x))))))))
   ((inc (inc zero)) (lambda (x) (<change> () x) (+ x 1)) 0))