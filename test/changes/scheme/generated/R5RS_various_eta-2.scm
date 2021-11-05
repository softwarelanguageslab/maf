; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((do-something (lambda ()
                         10))
         (id (lambda (y)
               (<change>
                  (do-something)
                  y)
               (<change>
                  y
                  (do-something))))
         (r1 ((id (lambda (a) a)) #t))
         (r2 ((id (lambda (b) b)) #f)))
   (<change>
      r1
      ((lambda (x) x) r1)))