; Changes:
; * removed: 1
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((make-flip (lambda ()
                      (let ((state 0))
                         (lambda ()
                            (<change>
                               (if (= state 0) (set! state 1) (set! state 0))
                               ())
                            state))))
         (flip (make-flip)))
   (<change>
      ()
      (display =))
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