; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((id (lambda (x)
               x))
         (blur (lambda (y)
                 (<change>
                    ()
                    y)
                 (<change>
                    ()
                    y)
                 (<change>
                    y
                    ((lambda (x) x) y))))
         (lp (lambda (a n)
               (if (<= n 1)
                  (id a)
                  (letrec ((r ((blur id) #t))
                           (s ((blur id) #f)))
                     (not ((blur lp) s (- n 1)))))))
         (res (lp #f 2)))
   (<change>
      res
      ((lambda (x) x) res)))