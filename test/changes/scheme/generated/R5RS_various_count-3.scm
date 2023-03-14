; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((count (lambda (n)
                  (if (<change> (= n 0) (not (= n 0)))
                     "done"
                     (count (- n 1))))))
   (count 10))