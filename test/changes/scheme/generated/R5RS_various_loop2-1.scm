; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((lp1 (lambda (i x)
                (letrec ((a (= 0 i)))
                   (if a
                      (<change>
                         x
                         (letrec ((lp2 (lambda (j f y)
                                         (letrec ((b (= 0 j)))
                                            (if b (lp1 (- i 1) y) (lp2 (- j 1) f (f y)))))))
                            (display n)
                            (lp2 10 (lambda (n) (+ n i)) x)))
                      (<change>
                         (letrec ((lp2 (lambda (j f y)
                                         (letrec ((b (= 0 j)))
                                            (if b (lp1 (- i 1) y) (lp2 (- j 1) f (f y)))))))
                            (lp2 10 (lambda (n) (+ n i)) x))
                         x))))))
   (lp1 10 0))