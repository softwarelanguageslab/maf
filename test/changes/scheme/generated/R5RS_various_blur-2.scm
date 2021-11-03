; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((id (lambda (x)
               (<change>
                  ()
                  x)
               x))
         (blur (lambda (y)
                 y))
         (lp (lambda (a n)
               (<change>
                  ()
                  (letrec ((r ((blur id) #t))
                           (s ((blur id) #f)))
                     (not ((blur lp) s (- n 1)))))
               (if (<= n 1)
                  (id a)
                  (letrec ((r ((blur id) #t))
                           (s ((blur id) #f)))
                     (not ((blur lp) s (- n 1)))))))
         (res (lp #f 2)))
   res)