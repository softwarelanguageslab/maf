; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((lp1 (lambda (i x)
                (letrec ((a (= 0 i)))
                   (if a
                      x
                      (letrec ((lp2 (lambda (j f y)
                                      (letrec ((b (= 0 j)))
                                         (if b (lp1 (- i 1) y) (lp2 (- j 1) f (f y)))))))
                         (lp2 10 (lambda (n) (+ n i)) x)))))))
   (<change>
      ()
      (display 0))
   (lp1 10 0))