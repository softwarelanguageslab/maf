; Changes:
; * removed: 2
; * added: 2
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (make-ring (lambda (n)
                      (let ((last (cons 0 ())))
                         (letrec ((build-list (lambda (n)
                                                (if (= n 0) last (cons n (build-list (- n 1)))))))
                            (let ((ring (build-list n)))
                               (<change>
                                  ()
                                  set-cdr!)
                               (set-cdr! last ring)
                               ring)))))
         (print-ring (lambda (r)
                       (<change>
                          ()
                          (display (begin (output " ") (output "..."))))
                       (letrec ((aux (lambda (l)
                                       (if (not (null? l))
                                          (if (<change> (eq? (cdr l) r) (not (eq? (cdr l) r)))
                                             (begin
                                                (output " ")
                                                (output (car l))
                                                (output "..."))
                                             (begin
                                                (output " ")
                                                (output (car l))
                                                (aux (cdr l))))
                                          #f))))
                          (<change>
                             (aux r)
                             ())
                          #t)))
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