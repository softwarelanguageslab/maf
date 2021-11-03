; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (make-ring (lambda (n)
                      (let ((last (cons 0 ())))
                         (letrec ((build-list (lambda (n)
                                                (if (= n 0) last (cons n (build-list (- n 1)))))))
                            (let ((ring (build-list n)))
                               (<change>
                                  (set-cdr! last ring)
                                  ring)
                               (<change>
                                  ring
                                  (set-cdr! last ring)))))))
         (print-ring (lambda (r)
                       (letrec ((aux (lambda (l)
                                       (if (not (null? l))
                                          (if (eq? (cdr l) r)
                                             (begin
                                                (output " ")
                                                (output (car l))
                                                (output "..."))
                                             (begin
                                                (output " ")
                                                (output (car l))
                                                (aux (cdr l))))
                                          #f))))
                          (aux r)
                          #t)))
         (r (make-ring 3)))
   (print-ring r)
   (<change>
      ()
      __toplevel_cons)
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