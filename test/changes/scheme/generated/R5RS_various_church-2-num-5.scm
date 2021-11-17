; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((zero (lambda (f x)
                 (<change>
                    ()
                    x)
                 x))
         (inc (lambda (n)
                (lambda (f x)
                   (f (n f x))))))
   ((inc (inc zero)) (lambda (x) (+ x 1)) 0))