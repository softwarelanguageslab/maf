; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((f (lambda (x)
              (letrec ((g (lambda (y)
                            (<change>
                               ()
                               (display +))
                            (+ x y))))
                 (<change>
                    ()
                    5)
                 (g 5)))))
   (= (f 0) 5))