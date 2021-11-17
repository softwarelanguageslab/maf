; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((g (lambda ()
              (<change>
                 ()
                 1)
              1))
         (f (lambda (n)
              (<change>
                 ()
                 (display n))
              (if (= n 0) 0 (+ (f (- n 1)) (g))))))
   (f 10))