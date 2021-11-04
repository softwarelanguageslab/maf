; Changes:
; * removed: 0
; * added: 0
; * swaps: 3
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
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
   (<change>
      (atom? '*abc$)
      (list? (__toplevel_cons 'atom ())))
   (<change>
      (list? (__toplevel_cons 'atom ()))
      (atom? '*abc$))
   (list? (__toplevel_cons 'atom (__toplevel_cons 'turkey (__toplevel_cons 'or ()))))
   (list?
      (__toplevel_cons (__toplevel_cons 'atom (__toplevel_cons 'turkey ())) (__toplevel_cons 'or ())))
   (list? ())
   (atom? ())
   (<change>
      (car (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
      (car
         (__toplevel_cons
            (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
            (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ()))))))
   (<change>
      (car
         (__toplevel_cons
            (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
            (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))
      (car (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))))
   (cdr (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
   (cdr
      (__toplevel_cons
         (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
         (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))
   (cons 'peanut (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ()))))
   (cons
      (__toplevel_cons 'banana (__toplevel_cons 'and ()))
      (__toplevel_cons
         'peanut
         (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ())))))
   (null? ()))