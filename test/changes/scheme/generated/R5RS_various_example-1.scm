; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 0
(letrec ((map (lambda (f lst)
                (if (null? lst)
                   ()
                   (cons (f (car lst)) (map f (cdr lst))))))
         (inc (lambda (n)
                (<change>
                   ()
                   (+ n 1))
                (<change>
                   ()
                   1)
                (<change>
                   ()
                   n)
                (+ n 1))))
   (map inc (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))))