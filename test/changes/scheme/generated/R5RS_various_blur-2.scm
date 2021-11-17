; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 3
(letrec ((id (lambda (x)
               x))
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
                     (if (<change> (<= n 1) (not (<= n 1)))
                        (id a)
                        (letrec ((r ((blur id) #t))
                                 (s ((blur id) #f)))
                           (<change>
                              (not ((blur lp) s (- n 1)))
                              ((lambda (x) x) (not ((blur lp) s (- n 1)))))))))))
         (res (lp #f 2)))
   (<change>
      res
      ((lambda (x) x) res)))