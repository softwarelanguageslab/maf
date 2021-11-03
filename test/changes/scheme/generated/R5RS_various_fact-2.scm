; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
(letrec ((fact (lambda (n)
                 (if (<change> (= n 0) (not (= n 0)))
                    1
                    (* n (fact (- n 1)))))))
   (fact 5))