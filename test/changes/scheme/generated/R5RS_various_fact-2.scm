; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((fact (lambda (n)
                 (if (= n 0)
                    (<change>
                       1
                       (* n (fact (- n 1))))
                    (<change>
                       (* n (fact (- n 1)))
                       1)))))
   (<change>
      ()
      (display (fact 5)))
   (<change>
      ()
      5)
   (<change>
      ()
      (display (fact 5)))
   (fact 5))