; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((the-lambda ())
         (update-lambda! (lambda (n)
                           (set! the-lambda (lambda ()
                                            n)))))
   (update-lambda! 0)
   (<change>
      (update-lambda! 1)
      ())
   (letrec ((res (the-lambda)))
      res))