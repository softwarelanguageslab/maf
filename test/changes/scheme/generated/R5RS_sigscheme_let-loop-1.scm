; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((loop (lambda (i l)
                 (let ((a 0)
                       (b 1)
                       (c 2))
                    (if (< i l) (loop (+ 1 i) l) l)))))
   (<change>
      ()
      20000)
   (loop 0 20000))