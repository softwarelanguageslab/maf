; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((loop (lambda (i l)
                 (let ((a 0)
                       (b 1)
                       (c 2))
                    (<change>
                       ()
                       (loop (+ 1 i) l))
                    (if (< i l) (loop (+ 1 i) l) l)))))
   (loop 0 20000))