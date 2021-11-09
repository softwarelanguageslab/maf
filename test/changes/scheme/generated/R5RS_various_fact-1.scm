; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((fact (lambda (n)
                 (<change>
                    (if (= n 0) 1 (* n (fact (- n 1))))
                    ((lambda (x) x) (if (<change> (= n 0) (not (= n 0))) 1 (* n (fact (- n 1)))))))))
   (<change>
      ()
      fact)
   (fact 5))