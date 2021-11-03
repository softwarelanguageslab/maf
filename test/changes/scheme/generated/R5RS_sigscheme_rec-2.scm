; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((test (lambda (f g n)
                 (<change>
                    ()
                    g)
                 (if (= n 0)
                    f
                    (let ((m (- n 1)))
                       ((f g f m) f g m)
                       ((g f g m) g f m)
                       g)))))
   (equal? (test test test 10) test))