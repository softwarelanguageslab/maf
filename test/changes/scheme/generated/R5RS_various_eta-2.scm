; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((do-something (lambda ()
                         (<change>
                            ()
                            (display 10))
                         10))
         (id (lambda (y)
               (do-something)
               y))
         (r1 ((id (lambda (a) a)) #t))
         (r2 ((id (lambda (b) b)) #f)))
   r1)