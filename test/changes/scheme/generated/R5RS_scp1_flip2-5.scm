; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((make-flip (lambda ()
                      (let ((state 0))
                         (lambda ()
                            (if (= state 0) (set! state 1) (set! state 0))
                            state))))
         (flip (make-flip)))
   (if (= (flip) 1)
      (if (= (flip) 0)
         (if (<change> (= (flip) 1) (not (= (flip) 1)))
            (= (flip) 0)
            #f)
         #f)
      #f))