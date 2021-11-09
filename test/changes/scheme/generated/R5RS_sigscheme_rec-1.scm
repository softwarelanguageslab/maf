; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((test (lambda (f g n)
                 (<change>
                    (if (= n 0)
                       f
                       (let ((m (- n 1)))
                          ((f g f m) f g m)
                          ((g f g m) g f m)
                          g))
                    ((lambda (x) x)
                       (if (= n 0)
                          f
                          (let ((m (- n 1)))
                             (<change>
                                ((f g f m) f g m)
                                ((g f g m) g f m))
                             (<change>
                                ((g f g m) g f m)
                                ((f g f m) f g m))
                             g)))))))
   (<change>
      ()
      (display test))
   (equal? (test test test 10) test))