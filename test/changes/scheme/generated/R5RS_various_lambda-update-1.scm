; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((the-lambda ())
         (update-lambda! (lambda (n)
                           (set! the-lambda (lambda ()
                                            (<change>
                                               ()
                                               (display n))
                                            n)))))
   (update-lambda! 0)
   (update-lambda! 1)
   (letrec ((res (the-lambda)))
      res))