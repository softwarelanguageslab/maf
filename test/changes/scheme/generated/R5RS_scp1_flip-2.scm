; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((flip (let ((state 0))
                 (lambda ()
                    (if (= state 0) (set! state 1) (set! state 0))
                    state))))
   (<change>
      (if (= (flip) 1)
         (if (= (flip) 0)
            (if (= (flip) 1) (= (flip) 0) #f)
            #f)
         #f)
      ((lambda (x) x)
         (if (<change> (= (flip) 1) (not (= (flip) 1)))
            (if (= (flip) 0)
               (if (= (flip) 1) (= (flip) 0) #f)
               #f)
            #f))))