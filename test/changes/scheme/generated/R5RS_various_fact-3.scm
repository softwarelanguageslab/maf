; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((fact (lambda (n)
                 (if (= n 0) 1 (* n (fact (- n 1)))))))
   (<change>
      ()
      (fact 5))
   (fact 5))