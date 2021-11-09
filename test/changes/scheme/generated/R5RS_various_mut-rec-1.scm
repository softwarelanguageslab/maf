; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((even? (lambda (x)
                  (<change>
                     (if (= x 0) #t (odd? (- x 1)))
                     ((lambda (x) x) (if (<change> (= x 0) (not (= x 0))) #t (odd? (- x 1)))))))
         (odd? (lambda (x)
                 (if (= x 0) #f (even? (- x 1))))))
   (even? 4))