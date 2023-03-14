; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((h (lambda (b)
              (letrec ((g (lambda (z)
                            z))
                       (f (lambda (k)
                            (if (<change> b (not b)) (k 1) (k 2))))
                       (y (f (lambda (x) x))))
                 (g y))))
         (x (h #t))
         (y (h #f)))
   y)