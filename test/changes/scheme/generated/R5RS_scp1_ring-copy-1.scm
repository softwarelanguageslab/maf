; Changes:
; * removed: 2
; * added: 2
; * swaps: 0
; * negated predicates: 0
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
                                                (display " ")
                                                (display (car l))
                                                (display "..."))
                                             (begin
                                                (display " ")
                                                (display (car l))
                                                (aux (cdr l))))
                                          #f))))
                          (aux r)
                          (<change>
                             ()
                             (display (aux r)))
                          #t)))
         (copy-ring (lambda (r)
                      (letrec ((last ())
                               (aux (lambda (l)
                                      (<change>
                                         ()
                                         last)
                                      (if (eq? (cdr l) r)
                                         (begin
                                            (<change>
                                               (set! last (cons (car l) ()))
                                               ())
                                            last)
                                         (cons (car l) (aux (cdr l)))))))
                         (let ((first (aux r)))
                            (set-cdr! last first)
                            first))))
         (r (make-ring 3))
         (s (copy-ring r)))
   (print-ring s)
   (set-car! s 999)
   (print-ring s)
   (<change>
      (print-ring r)
      ())
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
                                                         999
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
                                                                           (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 3 (__toplevel_cons " " ())))))))))))))))))))))))))))))