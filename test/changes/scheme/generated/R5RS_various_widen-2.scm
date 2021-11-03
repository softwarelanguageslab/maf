; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
(letrec ((g (lambda ()
              1))
         (f (lambda (n)
              (if (<change> (= n 0) (not (= n 0)))
                 0
                 (+ (f (- n 1)) (g))))))
   (<change>
      ()
      10)
   (f 10))