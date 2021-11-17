; Changes:
; * removed: 0
; * added: 1
; * swaps: 2
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((atom? (lambda (x)
                  (if (not (pair? x)) (not (null? x)) #f))))
   (atom? 'atom)
   (<change>
      (atom? 'turkey)
      (atom? 1942))
   (<change>
      (atom? 1942)
      (atom? 'turkey))
   (atom? 'u)
   (atom? '*abc$)
   (<change>
      (list? (__toplevel_cons 'atom ()))
      (list? (__toplevel_cons 'atom (__toplevel_cons 'turkey (__toplevel_cons 'or ())))))
   (<change>
      (list? (__toplevel_cons 'atom (__toplevel_cons 'turkey (__toplevel_cons 'or ()))))
      (list? (__toplevel_cons 'atom ())))
   (list?
      (__toplevel_cons (__toplevel_cons 'atom (__toplevel_cons 'turkey ())) (__toplevel_cons 'or ())))
   (list? ())
   (atom? ())
   (<change>
      ()
      __toplevel_cons)
   (car (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
   (car
      (__toplevel_cons
         (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
         (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))
   (cdr (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
   (<change>
      (cdr
         (__toplevel_cons
            (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
            (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))
      ((lambda (x) x)
         (cdr
            (__toplevel_cons
               (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
               (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))))
   (cons 'peanut (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ()))))
   (cons
      (__toplevel_cons 'banana (__toplevel_cons 'and ()))
      (__toplevel_cons
         'peanut
         (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ())))))
   (null? ()))