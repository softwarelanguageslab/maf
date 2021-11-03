; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((the-lambda ())
         (update-lambda! (lambda (n)
                           (set! the-lambda (lambda ()
                                            n)))))
   (update-lambda! 0)
   (<change>
      ()
      res)
   (update-lambda! 1)
   (letrec ((res (the-lambda)))
      res))