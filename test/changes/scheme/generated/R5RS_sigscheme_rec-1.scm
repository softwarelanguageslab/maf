; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((test (lambda (f g n)
                 (if (<change> (= n 0) (not (= n 0)))
                    f
                    (let ((m (- n 1)))
                       ((f g f m) f g m)
                       ((g f g m) g f m)
                       g)))))
   (equal? (test test test 10) test))