; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((id (lambda (x)
               x))
         (blur (lambda (y)
                 (<change>
                    ()
                    y)
                 y))
         (lp (lambda (a n)
               (if (<= n 1)
                  (id a)
                  (letrec ((r ((blur id) #t))
                           (s ((blur id) #f)))
                     (not ((blur lp) s (- n 1)))))))
         (res (lp #f 2)))
   res)