; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 1
(letrec ((test (lambda (f g n)
                 (<change>
                    ()
                    (- n 1))
                 (if (<change> (= n 0) (not (= n 0)))
                    f
                    (let ((m (- n 1)))
                       (<change>
                          ((f g f m) f g m)
                          ((g f g m) g f m))
                       (<change>
                          ((g f g m) g f m)
                          ((f g f m) f g m))
                       g)))))
   (equal? (test test test 10) test))