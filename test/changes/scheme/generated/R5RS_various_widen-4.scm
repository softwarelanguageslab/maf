; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((g (lambda ()
              1))
         (f (lambda (n)
              (if (<change> (= n 0) (not (= n 0)))
                 0
                 (+ (f (- n 1)) (g))))))
   (f 10))