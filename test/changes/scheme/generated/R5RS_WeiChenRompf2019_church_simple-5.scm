; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
(letrec ((pred (lambda (n)
                 (lambda (rf)
                    (lambda (rx)
                       (((n (lambda (g) (lambda (h) (h (g rf))))) (lambda (ignored) rx)) (lambda (id) id))))))
         (church0 (lambda (f0)
                    (lambda (x0)
                       x0)))
         (church1 (lambda (f1)
                    (lambda (x1)
                       (f1 x1))))
         (church0? (lambda (z)
                     ((z (lambda (zx) #f)) #t)))
         (ff (lambda (e)
               (if (<change> (church0? e) (not (church0? e)))
                  e
                  (ff ((church1 pred) e))))))
   (ff church1))