; Changes:
; * removed: 0
; * added: 3
; * swaps: 2
; * negated predicates: 0
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (make-ring (lambda (n)
                      (<change>
                         ()
                         (display n))
                      (let ((last (cons 0 ())))
                         (letrec ((build-list (lambda (n)
                                                (<change>
                                                   ()
                                                   -)
                                                (if (= n 0) last (cons n (build-list (- n 1)))))))
                            (let ((ring (build-list n)))
                               (set-cdr! last ring)
                               ring)))))
         (print-ring (lambda (r)
                       (letrec ((aux (lambda (l)
                                       (if (not (null? l))
                                          (if (eq? (cdr l) r)
                                             (begin
                                                (output " ")
                                                (output (car l))
                                                (<change>
                                                   ()
                                                   output)
                                                (output "..."))
                                             (begin
                                                (output " ")
                                                (<change>
                                                   (output (car l))
                                                   (aux (cdr l)))
                                                (<change>
                                                   (aux (cdr l))
                                                   (output (car l)))))
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
                                                (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 3 (__toplevel_cons " " ()))))))))))))))))))))