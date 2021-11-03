; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
(letrec ((id (lambda (x)
               x))
         (blur (lambda (y)
                 y))
         (lp (lambda (a n)
               (if (<change> (<= n 1) (not (<= n 1)))
                  (id a)
                  (letrec ((r ((blur id) #t))
                           (s ((blur id) #f)))
                     (<change>
                        ()
                        lp)
                     (not ((blur lp) s (- n 1)))))))
         (res (lp #f 2)))
   res)