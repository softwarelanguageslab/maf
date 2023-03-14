; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((*max* 20001)
         (test (lambda (x y)
                 (<change>
                    ()
                    (display *))
                 (if (= x *max*)
                    x
                    (test (- x (+ (* y 2) (/ x (abs y)))) (- y (+ (* x 2) (/ y (abs x)))))))))
   (test 1 1))