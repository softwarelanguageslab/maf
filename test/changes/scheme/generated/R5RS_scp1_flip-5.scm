; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
(letrec ((flip (let ((state 0))
                 (lambda ()
                    (<change>
                       ()
                       (set! state 1))
                    (if (<change> (= state 0) (not (= state 0)))
                       (set! state 1)
                       (set! state 0))
                    state))))
   (if (= (flip) 1)
      (if (= (flip) 0)
         (if (= (flip) 1) (= (flip) 0) #f)
         #f)
      #f))