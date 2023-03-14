; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((the-lambda ())
         (update-lambda! (lambda (n)
                           (set! the-lambda (lambda ()
                                            n)))))
   (<change>
      (update-lambda! 0)
      (update-lambda! 1))
   (<change>
      (update-lambda! 1)
      (update-lambda! 0))
   (letrec ((res (the-lambda)))
      res))