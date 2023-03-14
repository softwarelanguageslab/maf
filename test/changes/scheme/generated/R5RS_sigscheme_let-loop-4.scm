; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((loop (lambda (i l)
                 (let ((a 0)
                       (b 1)
                       (c 2))
                    (if (<change> (< i l) (not (< i l)))
                       (loop (+ 1 i) l)
                       l)))))
   (loop 0 20000))