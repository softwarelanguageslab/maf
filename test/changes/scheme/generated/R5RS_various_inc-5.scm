; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((inc (lambda (x)
                (+ x 1))))
   (<change>
      ()
      (display inc))
   (inc (inc 2)))