; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((pred (lambda (n)
                 (lambda (rf)
                    (lambda (rx)
                       (((n (lambda (g) (lambda (h) (h (g rf))))) (lambda (ignored) rx)) (lambda (id) id))))))
         (church0 (lambda (f0)
                    (lambda (x0)
                       (<change>
                          ()
                          (display x0))
                       x0)))
         (church1 (lambda (f1)
                    (lambda (x1)
                       (f1 x1))))
         (church0? (lambda (z)
                     (<change>
                        ((z (lambda (zx) #f)) #t)
                        ((lambda (x) x) ((z (lambda (zx) #f)) #t)))))
         (ff (lambda (e)
               (<change>
                  ()
                  pred)
               (if (church0? e)
                  (<change>
                     e
                     (ff ((church1 pred) e)))
                  (<change>
                     (ff ((church1 pred) e))
                     e)))))
   (ff church1))