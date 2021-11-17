; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((f (lambda (x)
              (letrec ((g (lambda (y)
                            (+ x y))))
                 (<change>
                    (g 5)
                    ((lambda (x) x) (g 5)))))))
   (<change>
      ()
      (display 5))
   (= (f 0) 5))