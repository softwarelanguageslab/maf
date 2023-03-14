; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((pred (lambda (n)
                 (lambda (rf)
                    (<change>
                       (lambda (rx)
                          (((n (lambda (g) (lambda (h) (h (g rf))))) (lambda (ignored) rx)) (lambda (id) id)))
                       ((lambda (x) x)
                          (lambda (rx)
                             (((n (lambda (g) (lambda (h) (h (g rf))))) (lambda (ignored) rx)) (lambda (id) id))))))))
         (church0 (lambda (f0)
                    (lambda (x0)
                       x0)))
         (church1 (lambda (f1)
                    (lambda (x1)
                       (f1 x1))))
         (church0? (lambda (z)
                     ((z (lambda (zx) #f)) #t)))
         (ff (lambda (e)
               (if (church0? e)
                  (<change>
                     e
                     (ff ((church1 pred) e)))
                  (<change>
                     (ff ((church1 pred) e))
                     e)))))
   (<change>
      ()
      (display ff))
   (ff church1))