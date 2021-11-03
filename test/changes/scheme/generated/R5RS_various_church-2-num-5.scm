; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((zero (lambda (f x)
                 (<change>
                    ()
                    (display x))
                 x))
         (inc (lambda (n)
                (lambda (f x)
                   (f (n f x))))))
   (<change>
      ((inc (inc zero)) (lambda (x) (+ x 1)) 0)
      ((lambda (x) x) ((inc (inc zero)) (lambda (x) (+ x 1)) 0))))