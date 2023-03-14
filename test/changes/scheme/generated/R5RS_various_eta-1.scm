; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((do-something (lambda ()
                         (<change>
                            10
                            ((lambda (x) x) 10))))
         (id (lambda (y)
               (do-something)
               (<change>
                  ()
                  y)
               y))
         (r1 ((id (lambda (a) a)) #t))
         (r2 ((id (lambda (b) b)) #f)))
   r1)