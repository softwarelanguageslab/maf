; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((rotate (lambda (n x y z)
                   (if (= n 0) x (rotate (- n 1) y z x)))))
   (<change>
      ()
      5)
   (<change>
      ()
      rotate)
   (rotate 41 5 #t "hallo"))