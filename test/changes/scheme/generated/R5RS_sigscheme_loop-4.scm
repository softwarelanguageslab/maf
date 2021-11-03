; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
(letrec ((loop (lambda (i l)
                 (if (<change> (< i l) (not (< i l)))
                    (loop (+ 1 i) l)
                    l))))
   (loop 0 8000))