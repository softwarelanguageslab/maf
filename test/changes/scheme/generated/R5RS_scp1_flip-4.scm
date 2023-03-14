; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((flip (let ((state 0))
                 (lambda ()
                    (if (= state 0) (set! state 1) (set! state 0))
                    (<change>
                       ()
                       1)
                    state))))
   (if (= (flip) 1)
      (if (= (flip) 0)
         (if (= (flip) 1) (= (flip) 0) #f)
         #f)
      #f))