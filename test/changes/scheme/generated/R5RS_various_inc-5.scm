; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((inc (lambda (x)
                (<change>
                   (+ x 1)
                   ((lambda (x) x) (+ x 1))))))
   (<change>
      ()
      (inc 2))
   (inc (inc 2)))