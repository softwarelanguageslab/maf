; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (make-ring (lambda (n)
                      (let ((last (cons 0 ())))
                         (letrec ((build-list (lambda (n)
                                                (if (= n 0) last (cons n (build-list (- n 1)))))))
                            (let ((ring (build-list n)))
                               (set-cdr! last ring)
                               ring)))))
         (print-ring (lambda (r)
                       (letrec ((aux (lambda (l)
                                       (if (not (null? l))
                                          (if (eq? (cdr l) r)
                                             (<change>
                                                (begin
                                                   (output " ")
                                                   (output (car l))
                                                   (output "..."))
                                                (begin
                                                   aux
                                                   (output " ")
                                                   (output (car l))
                                                   (aux (cdr l))))
                                             (<change>
                                                (begin
                                                   (output " ")
                                                   (output (car l))
                                                   (aux (cdr l)))
                                                (begin
                                                   (output " ")
                                                   (output (car l))
                                                   (output "..."))))
                                          #f))))
                          (<change>
                             (aux r)
                             #t)
                          (<change>
                             #t
                             (aux r)))))
         (r (make-ring 3)))
   (print-ring r)
   (print-ring (cdr r))
   (<change>
      (equal?
         result
         (__toplevel_cons
            "..."
            (__toplevel_cons
               3
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     0
                     (__toplevel_cons
                        " "
                        (__toplevel_cons
                           1
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 2
                                 (__toplevel_cons
                                    " "
                                    (__toplevel_cons
                                       "..."
                                       (__toplevel_cons
                                          0
                                          (__toplevel_cons
                                             " "
                                             (__toplevel_cons
                                                1
                                                (__toplevel_cons
                                                   " "
                                                   (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 3 (__toplevel_cons " " ())))))))))))))))))))
      ((lambda (x) x)
         (equal?
            result
            (__toplevel_cons
               "..."
               (__toplevel_cons
                  3
                  (__toplevel_cons
                     " "
                     (__toplevel_cons
                        0
                        (__toplevel_cons
                           " "
                           (__toplevel_cons
                              1
                              (__toplevel_cons
                                 " "
                                 (__toplevel_cons
                                    2
                                    (__toplevel_cons
                                       " "
                                       (__toplevel_cons
                                          "..."
                                          (__toplevel_cons
                                             0
                                             (__toplevel_cons
                                                " "
                                                (__toplevel_cons
                                                   1
                                                   (__toplevel_cons
                                                      " "
                                                      (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 3 (__toplevel_cons " " ()))))))))))))))))))))))