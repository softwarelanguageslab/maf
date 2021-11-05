; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((loop (lambda (i l)
                 (<change>
                    ()
                    loop)
                 (<change>
                    (if (< i l) (loop (+ 1 i) l) l)
                    ((lambda (x) x) (if (< i l) (loop (+ 1 i) l) l))))))
   (loop 0 8000))