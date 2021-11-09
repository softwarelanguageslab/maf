; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((do-something (lambda ()
                         10))
         (id (lambda (y)
               (<change>
                  (do-something)
                  ())
               y))
         (r1 ((id (lambda (a) a)) #t))
         (r2 ((id (lambda (b) b)) #f)))
   r1)