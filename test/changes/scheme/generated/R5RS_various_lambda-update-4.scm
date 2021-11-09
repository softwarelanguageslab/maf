; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((the-lambda ())
         (update-lambda! (lambda (n)
                           (<change>
                              (set! the-lambda (lambda ()
                                               n))
                              ((lambda (x) x) (set! the-lambda (lambda () n)))))))
   (update-lambda! 0)
   (<change>
      ()
      (display 1))
   (update-lambda! 1)
   (<change>
      (letrec ((res (the-lambda)))
         res)
      ((lambda (x) x) (letrec ((res (the-lambda))) res))))