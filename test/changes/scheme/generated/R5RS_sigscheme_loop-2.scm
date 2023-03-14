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
                    l)
                 (if (< i l) (loop (+ 1 i) l) l))))
   (<change>
      (loop 0 8000)
      ((lambda (x) x) (loop 0 8000))))