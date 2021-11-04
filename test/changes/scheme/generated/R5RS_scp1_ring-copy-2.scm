; Changes:
; * removed: 1
; * added: 0
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (make-ring (lambda (n)
                      (let ((last (cons 0 ())))
                         (letrec ((build-list (lambda (n)
                                                (if (<change> (= n 0) (not (= n 0)))
                                                   last
                                                   (cons n (build-list (- n 1)))))))
                            (let ((ring (build-list n)))
                               (<change>
                                  (set-cdr! last ring)
                                  ())
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
                          #t)))
         (copy-ring (lambda (r)
                      (letrec ((last ())
                               (aux (lambda (l)
                                      (if (eq? (cdr l) r)
                                         (begin
                                            (set! last (cons (car l) ()))
                                            last)
                                         (cons (car l) (aux (cdr l)))))))
                         (let ((first (aux r)))
                            (set-cdr! last first)
                            first))))
         (r (make-ring 3))
         (s (copy-ring r)))
   (print-ring s)
   (set-car! s 999)
   (<change>
      (print-ring s)
      (print-ring r))
   (<change>
      (print-ring r)
      (print-ring s))
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