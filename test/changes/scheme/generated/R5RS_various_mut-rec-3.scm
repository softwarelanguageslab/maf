; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((even? (lambda (x)
                  (if (<change> (= x 0) (not (= x 0)))
                     #t
                     (odd? (- x 1)))))
         (odd? (lambda (x)
                 (<change>
                    ()
                    x)
                 (if (= x 0)
                    (<change>
                       #f
                       (even? (- x 1)))
                    (<change>
                       (even? (- x 1))
                       #f)))))
   (even? 4))