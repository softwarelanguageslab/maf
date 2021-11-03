; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 0
(letrec ((f (lambda (x)
              (letrec ((g (lambda (y)
                            (<change>
                               ()
                               +)
                            (+ x y))))
                 (g 5)))))
   (<change>
      ()
      =)
   (<change>
      ()
      (display 0))
   (= (f 0) 5))