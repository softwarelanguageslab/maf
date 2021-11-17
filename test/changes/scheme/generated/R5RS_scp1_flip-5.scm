; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((flip (let ((state 0))
                 (<change>
                    (lambda ()
                       (if (= state 0) (set! state 1) (set! state 0))
                       state)
                    ((lambda (x) x)
                       (lambda ()
                          (<change>
                             ()
                             state)
                          (if (= state 0) (set! state 1) (set! state 0))
                          state))))))
   (if (= (flip) 1)
      (if (= (flip) 0)
         (if (= (flip) 1) (= (flip) 0) #f)
         #f)
      #f))