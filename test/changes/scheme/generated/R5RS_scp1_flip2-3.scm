; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((make-flip (lambda ()
                      (let ((state 0))
                         (lambda ()
                            (<change>
                               (if (= state 0) (set! state 1) (set! state 0))
                               state)
                            (<change>
                               state
                               (if (= state 0) (set! state 1) (set! state 0)))))))
         (flip (make-flip)))
   (if (= (flip) 1)
      (if (= (flip) 0)
         (if (= (flip) 1) (= (flip) 0) #f)
         #f)
      #f))