; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((loop (lambda (i l)
                 (<change>
                    ()
                    (+ 1 i))
                 (if (< i l) (loop (+ 1 i) l) l))))
   (loop 0 8000))