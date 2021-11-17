; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((count (lambda (n)
                  (<change>
                     ()
                     (display n))
                  (if (= n 0) "done" (count (- n 1))))))
   (count 10))