; Changes:
; * removed: 2
; * added: 4
; * swaps: 2
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
                       (<change>
                          ()
                          (display (display (car l))))
                       (letrec ((aux (lambda (l)
                                       (if (not (null? l))
                                          (if (eq? (cdr l) r)
                                             (begin
                                                (display " ")
                                                (display (car l))
                                                (display "..."))
                                             (begin
                                                (<change>
                                                   (display " ")
                                                   ())
                                                (display (car l))
                                                (aux (cdr l))))
                                          #f))))
                          (<change>
                             (aux r)
                             ())
                          #t)))
         (copy-ring (lambda (r)
                      (letrec ((last ())
                               (aux (lambda (l)
                                      (if (eq? (cdr l) r)
                                         (begin
                                            (<change>
                                               ()
                                               (display ()))
                                            (<change>
                                               (set! last (cons (car l) ()))
                                               last)
                                            (<change>
                                               last
                                               (set! last (cons (car l) ()))))
                                         (cons (car l) (aux (cdr l)))))))
                         (<change>
                            ()
                            (display aux))
                         (let ((first (aux r)))
                            (set-cdr! last first)
                            (<change>
                               ()
                               set-cdr!)
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