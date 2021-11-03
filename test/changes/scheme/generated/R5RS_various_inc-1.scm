; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((inc (lambda (x)
                (<change>
                   ()
                   (display 1))
                (+ x 1))))
   (inc (inc 2)))