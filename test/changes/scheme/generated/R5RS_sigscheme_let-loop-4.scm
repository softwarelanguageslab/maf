; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((loop (lambda (i l)
                 (<change>
                    ()
                    i)
                 (let ((a 0)
                       (b 1)
                       (c 2))
                    (if (< i l) (loop (+ 1 i) l) l)))))
   (loop 0 20000))