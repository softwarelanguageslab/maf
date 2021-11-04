; Changes:
; * removed: 0
; * added: 5
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 3
(letrec ((result ())
         (output (lambda (i)
                   (<change>
                      (set! result (cons i result))
                      ((lambda (x) x) (set! result (cons i result))))))
         (make-ring (lambda (n)
                      (let ((last (cons 0 ())))
                         (<change>
                            ()
                            (set-cdr! last ring))
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
                                                (<change>
                                                   ()
                                                   l)
                                                (display (car l))
                                                (display "..."))
                                             (begin
                                                (<change>
                                                   ()
                                                   aux)
                                                (display " ")
                                                (display (car l))
                                                (aux (cdr l))))
                                          #f))))
                          (<change>
                             ()
                             r)
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
                         (<change>
                            (let ((first (aux r)))
                               (set-cdr! last first)
                               first)
                            ((lambda (x) x) (let ((first (aux r))) (set-cdr! last first) first))))))
         (r (make-ring 3))
         (s (copy-ring r)))
   (<change>
      (print-ring s)
      ((lambda (x) x) (print-ring s)))
   (<change>
      ()
      "...")
   (<change>
      (set-car! s 999)
      (print-ring s))
   (<change>
      (print-ring s)
      (set-car! s 999))
   (print-ring r)
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