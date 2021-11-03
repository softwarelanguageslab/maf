; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((g (lambda ()
              (<change>
                 ()
                 (display 1))
              1))
         (f (lambda (n)
              (if (= n 0) 0 (+ (f (- n 1)) (g))))))
   (f 10))