; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((flip (let ((state 0))
                 (lambda ()
                    (if (<change> (= state 0) (not (= state 0)))
                       (set! state 1)
                       (set! state 0))
                    state))))
   (if (= (flip) 1)
      (if (= (flip) 0)
         (if (= (flip) 1)
            (<change>
               (= (flip) 0)
               #f)
            (<change>
               #f
               (= (flip) 0)))
         #f)
      #f))