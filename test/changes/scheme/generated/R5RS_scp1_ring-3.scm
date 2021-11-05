; Changes:
; * removed: 1
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 4
(letrec ((result ())
         (output (lambda (i)
                   (<change>
                      (set! result (cons i result))
                      ((lambda (x) x) (set! result (cons i result))))))
         (make-ring (lambda (n)
                      (let ((last (cons 0 ())))
                         (letrec ((build-list (lambda (n)
                                                (if (= n 0) last (cons n (build-list (- n 1)))))))
                            (let ((ring (build-list n)))
                               (set-cdr! last ring)
                               (<change>
                                  ()
                                  (display ring))
                               (<change>
                                  ring
                                  ((lambda (x) x) ring)))))))
         (print-ring (lambda (r)
                       (letrec ((aux (lambda (l)
                                       (<change>
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
                                             #f)
                                          ((lambda (x) x)
                                             (if (not (null? l))
                                                (if (eq? (cdr l) r)
                                                   (<change>
                                                      (begin
                                                         (output " ")
                                                         (output (car l))
                                                         (output "..."))
                                                      (begin
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
                                                         (output "..."))))
                                                #f))))))
                          (aux r)
                          (<change>
                             #t
                             ((lambda (x) x) #t)))))
         (r (make-ring 3)))
   (print-ring r)
   (<change>
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
      (print-ring (cdr r))))