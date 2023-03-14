; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((test (lambda (f g n)
                 (if (= n 0)
                    f
                    (let ((m (- n 1)))
                       ((f g f m) f g m)
                       (<change>
                          ()
                          g)
                       ((g f g m) g f m)
                       (<change>
                          g
                          ((lambda (x) x) g)))))))
   (equal? (test test test 10) test))