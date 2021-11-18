; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((even? (lambda (x)
                  (<change>
                     ()
                     odd?)
                  (if (= x 0) #t (odd? (- x 1)))))
         (odd? (lambda (x)
                 (if (= x 0) #f (even? (- x 1))))))
   (even? 4))