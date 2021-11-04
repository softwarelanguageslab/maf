; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 3
(letrec ((pred (lambda (n)
                 (lambda (rf)
                    (<change>
                       (lambda (rx)
                          (((n (lambda (g) (lambda (h) (h (g rf))))) (lambda (ignored) rx)) (lambda (id) id)))
                       ((lambda (x) x)
                          (lambda (rx)
                             (((n (lambda (g) (lambda (h) (h (g rf))))) (lambda (ignored) rx)) (lambda (id) id))))))))
         (church0 (lambda (f0)
                    (<change>
                       (lambda (x0)
                          x0)
                       ((lambda (x) x) (lambda (x0) x0)))))
         (church1 (lambda (f1)
                    (lambda (x1)
                       (f1 x1))))
         (church0? (lambda (z)
                     (<change>
                        ()
                        z)
                     ((z (lambda (zx) #f)) #t)))
         (ff (lambda (e)
               (<change>
                  (if (church0? e) e (ff ((church1 pred) e)))
                  ((lambda (x) x) (if (church0? e) e (ff ((church1 pred) e))))))))
   (ff church1))