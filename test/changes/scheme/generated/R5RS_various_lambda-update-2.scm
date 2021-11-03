; Changes:
; * removed: 0
; * added: 2
; * swaps: 1
; * negated predicates: 0
(letrec ((the-lambda ())
         (update-lambda! (lambda (n)
                           (set! the-lambda (lambda ()
                                            n)))))
   (update-lambda! 0)
   (<change>
      ()
      (display (letrec ((res (the-lambda))) res res)))
   (<change>
      (update-lambda! 1)
      (letrec ((res (the-lambda)))
         res))
   (<change>
      (letrec ((res (the-lambda)))
         res)
      (update-lambda! 1)))