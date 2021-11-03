; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((f (lambda (x)
              (<change>
                 ()
                 y)
              (letrec ((g (lambda (y)
                            (+ x y))))
                 (g 5)))))
   (= (f 0) 5))