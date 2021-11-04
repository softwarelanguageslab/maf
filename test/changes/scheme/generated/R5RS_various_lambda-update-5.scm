; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((the-lambda ())
         (update-lambda! (lambda (n)
                           (set! the-lambda (lambda ()
                                            (<change>
                                               ()
                                               n)
                                            n)))))
   (update-lambda! 0)
   (update-lambda! 1)
   (letrec ((res (the-lambda)))
      (<change>
         ()
         res)
      res))