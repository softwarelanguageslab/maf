; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((my-cons (lambda (el lst)
                    (<change>
                       ()
                       lst)
                    (cons el lst)))
         (my-list (my-cons 1 (my-cons 2 (my-cons 3 ())))))
   my-list)