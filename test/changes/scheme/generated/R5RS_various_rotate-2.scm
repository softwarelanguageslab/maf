; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
(letrec ((rotate (lambda (n x y z)
                   (if (<change> (= n 0) (not (= n 0)))
                      x
                      (rotate (- n 1) y z x)))))
   (rotate 41 5 #t "hallo"))