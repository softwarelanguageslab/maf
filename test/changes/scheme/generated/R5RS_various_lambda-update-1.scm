; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((the-lambda ())
         (update-lambda! (lambda (n)
                           (<change>
                              ()
                              n)
                           (set! the-lambda (lambda ()
                                            n)))))
   (update-lambda! 0)
   (update-lambda! 1)
   (letrec ((res (the-lambda)))
      (<change>
         res
         ((lambda (x) x) res))))