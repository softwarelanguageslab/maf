; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 2
(letrec ((flip (let ((state 0))
                 (lambda ()
                    (if (= state 0) (set! state 1) (set! state 0))
                    state))))
   (if (<change> (= (flip) 1) (not (= (flip) 1)))
      (if (<change> (= (flip) 0) (not (= (flip) 0)))
         (if (= (flip) 1) (= (flip) 0) #f)
         #f)
      #f))