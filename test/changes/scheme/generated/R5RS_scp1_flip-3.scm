; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((flip (let ((state 0))
                 (lambda ()
                    (if (= state 0)
                       (<change>
                          (set! state 1)
                          (set! state 0))
                       (<change>
                          (set! state 0)
                          (set! state 1)))
                    state))))
   (<change>
      ()
      =)
   (if (= (flip) 1)
      (if (= (flip) 0)
         (if (= (flip) 1) (= (flip) 0) #f)
         #f)
      #f))