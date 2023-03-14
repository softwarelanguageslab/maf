; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((my-cons (lambda (el lst)
                    (<change>
                       (cons el lst)
                       ((lambda (x) x) (cons el lst)))))
         (my-list (my-cons 1 (my-cons 2 (my-cons 3 ())))))
   (<change>
      ()
      my-list)
   my-list)