; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 0
(letrec ((fact (lambda (n)
                 (if (= n 0) 1 (* n (fact (- n 1)))))))
   (fact 5))