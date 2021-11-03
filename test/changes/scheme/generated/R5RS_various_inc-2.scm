; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 0
(letrec ((inc (lambda (x)
                (+ x 1))))
   (<change>
      ()
      2)
   (<change>
      ()
      (inc (inc 2)))
   (<change>
      ()
      (display (inc (inc 2))))
   (inc (inc 2)))