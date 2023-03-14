; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((rotate (lambda (n x y z)
                   (if (= n 0)
                      (<change>
                         x
                         (rotate (- n 1) y z x))
                      (<change>
                         (rotate (- n 1) y z x)
                         x)))))
   (<change>
      ()
      "hallo")
   (rotate 41 5 #t "hallo"))