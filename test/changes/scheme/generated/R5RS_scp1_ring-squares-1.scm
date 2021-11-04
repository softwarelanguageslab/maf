; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (kw-lijst (lambda (lst)
                     (letrec ((loop (lambda (l)
                                      (let ((rest (cdr l))
                                            (n (list (* (car l) (car l)))))
                                         (set-cdr! l n)
                                         (set-cdr! n rest)
                                         (if (not (eq? rest lst)) (loop rest) #f)))))
                        (loop lst)
                        lst)))
         (print-ring (lambda (r)
                       (letrec ((aux (lambda (l)
                                       (if (not (null? l))
                                          (if (eq? (cdr l) r)
                                             (begin
                                                (output " ")
                                                (output (car l))
                                                (<change>
                                                   ()
                                                   (car l))
                                                (output "..."))
                                             (begin
                                                (<change>
                                                   (output " ")
                                                   (output (car l)))
                                                (<change>
                                                   (output (car l))
                                                   (output " "))
                                                (aux (cdr l))))
                                          #f))))
                          (aux r)
                          (<change>
                             #t
                             ((lambda (x) x) #t)))))
         (last-cons (cons 3 ()))
         (test-lst (cons 1 (cons 4 last-cons))))
   (set-cdr! last-cons test-lst)
   (print-ring (kw-lijst test-lst))
   (equal?
      result
      (__toplevel_cons
         "..."
         (__toplevel_cons
            9
            (__toplevel_cons
               " "
               (__toplevel_cons
                  3
                  (__toplevel_cons
                     " "
                     (__toplevel_cons
                        16
                        (__toplevel_cons
                           " "
                           (__toplevel_cons
                              4
                              (__toplevel_cons
                                 " "
                                 (__toplevel_cons 1 (__toplevel_cons " " (__toplevel_cons 1 (__toplevel_cons " " ())))))))))))))))