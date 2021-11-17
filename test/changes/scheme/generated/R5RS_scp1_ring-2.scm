; Changes:
; * removed: 1
; * added: 4
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 2
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (make-ring (lambda (n)
                      (let ((last (cons 0 ())))
                         (letrec ((build-list (lambda (n)
                                                (if (= n 0)
                                                   (<change>
                                                      last
                                                      (cons n (build-list (- n 1))))
                                                   (<change>
                                                      (cons n (build-list (- n 1)))
                                                      last)))))
                            (<change>
                               (let ((ring (build-list n)))
                                  (set-cdr! last ring)
                                  ring)
                               ((lambda (x) x) (let ((ring (build-list n))) (<change> (set-cdr! last ring) ()) ring)))))))
         (print-ring (lambda (r)
                       (letrec ((aux (lambda (l)
                                       (if (not (null? l))
                                          (if (eq? (cdr l) r)
                                             (begin
                                                (<change>
                                                   ()
                                                   (display output))
                                                (output " ")
                                                (output (car l))
                                                (<change>
                                                   ()
                                                   (display output))
                                                (output "..."))
                                             (begin
                                                (<change>
                                                   (output " ")
                                                   ((lambda (x) x) (output " ")))
                                                (<change>
                                                   ()
                                                   (cdr l))
                                                (output (car l))
                                                (aux (cdr l))))
                                          #f))))
                          (aux r)
                          #t)))
         (r (make-ring 3)))
   (print-ring r)
   (print-ring (cdr r))
   (<change>
      ()
      (display "..."))
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