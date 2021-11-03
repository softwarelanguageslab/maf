; Changes:
; * removed: 1
; * added: 1
; * swaps: 0
; * negated predicates: 1
(letrec ((flip (let ((state 0))
                 (lambda ()
                    (<change>
                       ()
                       state)
                    (<change>
                       (if (= state 0) (set! state 1) (set! state 0))
                       ())
                    state))))
   (if (= (flip) 1)
      (if (= (flip) 0)
         (if (<change> (= (flip) 1) (not (= (flip) 1)))
            (= (flip) 0)
            #f)
         #f)
      #f))