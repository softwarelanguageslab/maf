; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((id (lambda (x)
               (<change>
                  ()
                  x)
               (<change>
                  x
                  ((lambda (x) x) x))))
         (blur (lambda (y)
                 y))
         (lp (lambda (a n)
               (<change>
                  (if (<= n 1)
                     (id a)
                     (letrec ((r ((blur id) #t))
                              (s ((blur id) #f)))
                        (not ((blur lp) s (- n 1)))))
                  ((lambda (x) x)
                     (if (<= n 1)
                        (id a)
                        (letrec ((r ((blur id) #t))
                                 (s ((blur id) #f)))
                           (not ((blur lp) s (- n 1)))))))))
         (res (lp #f 2)))
   res)