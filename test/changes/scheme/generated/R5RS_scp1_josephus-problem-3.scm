; Changes:
; * removed: 0
; * added: 2
; * swaps: 4
; * negated predicates: 2
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
                                             (begin
                                                (output " ")
                                                (<change>
                                                   ()
                                                   (display output))
                                                (<change>
                                                   (output (car l))
                                                   (output "..."))
                                                (<change>
                                                   (output "...")
                                                   (output (car l))))
                                             (begin
                                                (output " ")
                                                (<change>
                                                   (output (car l))
                                                   (aux (cdr l)))
                                                (<change>
                                                   (aux (cdr l))
                                                   (output (car l)))))
                                          #f))))
                          (aux r)
                          #t)))
         (copy-ring (lambda (r)
                      (letrec ((last ())
                               (aux (lambda (l)
                                      (if (eq? (cdr l) r)
                                         (begin
                                            (<change>
                                               ()
                                               car)
                                            (set! last (cons (car l) ()))
                                            last)
                                         (cons (car l) (aux (cdr l)))))))
                         (let ((first (aux r)))
                            (set-cdr! last first)
                            first))))
         (right-rotate (lambda (r)
                         (letrec ((iter (lambda (l)
                                          (if (eq? (cdr l) r) l (iter (cdr l))))))
                            (iter r))))
         (Josephus (lambda (r n)
                     (letrec ((remove-nth! (lambda (l n)
                                             (if (<change> (<= n 2) (not (<= n 2)))
                                                (begin
                                                   (<change>
                                                      (set-cdr! l (cddr l))
                                                      (cdr l))
                                                   (<change>
                                                      (cdr l)
                                                      (set-cdr! l (cddr l))))
                                                (remove-nth! (cdr l) (- n 1)))))
                              (iter (lambda (l)
                                      (print-ring l)
                                      (if (<change> (eq? l (cdr l)) (not (eq? l (cdr l))))
                                         (car l)
                                         (iter (remove-nth! l n))))))
                        (if (= n 1)
                           (car (right-rotate r))
                           (iter (copy-ring r))))))
         (ring (make-ring 5)))
   (Josephus ring 5)
   (<change>
      (print-ring ring)
      (equal?
         result
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
                        (__toplevel_cons
                           2
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 3
                                 (__toplevel_cons
                                    " "
                                    (__toplevel_cons
                                       4
                                       (__toplevel_cons
                                          " "
                                          (__toplevel_cons
                                             5
                                             (__toplevel_cons
                                                " "
                                                (__toplevel_cons
                                                   "..."
                                                   (__toplevel_cons
                                                      5
                                                      (__toplevel_cons
                                                         " "
                                                         (__toplevel_cons
                                                            "..."
                                                            (__toplevel_cons
                                                               5
                                                               (__toplevel_cons
                                                                  " "
                                                                  (__toplevel_cons
                                                                     3
                                                                     (__toplevel_cons
                                                                        " "
                                                                        (__toplevel_cons
                                                                           "..."
                                                                           (__toplevel_cons
                                                                              3
                                                                              (__toplevel_cons
                                                                                 " "
                                                                                 (__toplevel_cons
                                                                                    4
                                                                                    (__toplevel_cons
                                                                                       " "
                                                                                       (__toplevel_cons
                                                                                          5
                                                                                          (__toplevel_cons
                                                                                             " "
                                                                                             (__toplevel_cons
                                                                                                "..."
                                                                                                (__toplevel_cons
                                                                                                   3
                                                                                                   (__toplevel_cons
                                                                                                      " "
                                                                                                      (__toplevel_cons
                                                                                                         4
                                                                                                         (__toplevel_cons
                                                                                                            " "
                                                                                                            (__toplevel_cons
                                                                                                               5
                                                                                                               (__toplevel_cons
                                                                                                                  " "
                                                                                                                  (__toplevel_cons
                                                                                                                     0
                                                                                                                     (__toplevel_cons
                                                                                                                        " "
                                                                                                                        (__toplevel_cons
                                                                                                                           "..."
                                                                                                                           (__toplevel_cons
                                                                                                                              2
                                                                                                                              (__toplevel_cons
                                                                                                                                 " "
                                                                                                                                 (__toplevel_cons
                                                                                                                                    3
                                                                                                                                    (__toplevel_cons
                                                                                                                                       " "
                                                                                                                                       (__toplevel_cons
                                                                                                                                          4
                                                                                                                                          (__toplevel_cons
                                                                                                                                             " "
                                                                                                                                             (__toplevel_cons
                                                                                                                                                5
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   " "
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      0
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
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           2
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              " "
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 3
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    " "
                                                                                                                                                                                    (__toplevel_cons 4 (__toplevel_cons " " (__toplevel_cons 5 (__toplevel_cons " " ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   (<change>
      (equal?
         result
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
                        (__toplevel_cons
                           2
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 3
                                 (__toplevel_cons
                                    " "
                                    (__toplevel_cons
                                       4
                                       (__toplevel_cons
                                          " "
                                          (__toplevel_cons
                                             5
                                             (__toplevel_cons
                                                " "
                                                (__toplevel_cons
                                                   "..."
                                                   (__toplevel_cons
                                                      5
                                                      (__toplevel_cons
                                                         " "
                                                         (__toplevel_cons
                                                            "..."
                                                            (__toplevel_cons
                                                               5
                                                               (__toplevel_cons
                                                                  " "
                                                                  (__toplevel_cons
                                                                     3
                                                                     (__toplevel_cons
                                                                        " "
                                                                        (__toplevel_cons
                                                                           "..."
                                                                           (__toplevel_cons
                                                                              3
                                                                              (__toplevel_cons
                                                                                 " "
                                                                                 (__toplevel_cons
                                                                                    4
                                                                                    (__toplevel_cons
                                                                                       " "
                                                                                       (__toplevel_cons
                                                                                          5
                                                                                          (__toplevel_cons
                                                                                             " "
                                                                                             (__toplevel_cons
                                                                                                "..."
                                                                                                (__toplevel_cons
                                                                                                   3
                                                                                                   (__toplevel_cons
                                                                                                      " "
                                                                                                      (__toplevel_cons
                                                                                                         4
                                                                                                         (__toplevel_cons
                                                                                                            " "
                                                                                                            (__toplevel_cons
                                                                                                               5
                                                                                                               (__toplevel_cons
                                                                                                                  " "
                                                                                                                  (__toplevel_cons
                                                                                                                     0
                                                                                                                     (__toplevel_cons
                                                                                                                        " "
                                                                                                                        (__toplevel_cons
                                                                                                                           "..."
                                                                                                                           (__toplevel_cons
                                                                                                                              2
                                                                                                                              (__toplevel_cons
                                                                                                                                 " "
                                                                                                                                 (__toplevel_cons
                                                                                                                                    3
                                                                                                                                    (__toplevel_cons
                                                                                                                                       " "
                                                                                                                                       (__toplevel_cons
                                                                                                                                          4
                                                                                                                                          (__toplevel_cons
                                                                                                                                             " "
                                                                                                                                             (__toplevel_cons
                                                                                                                                                5
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   " "
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      0
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
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           2
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              " "
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 3
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    " "
                                                                                                                                                                                    (__toplevel_cons 4 (__toplevel_cons " " (__toplevel_cons 5 (__toplevel_cons " " ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      (print-ring ring)))