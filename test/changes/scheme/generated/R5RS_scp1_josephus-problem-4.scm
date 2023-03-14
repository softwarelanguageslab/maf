; Changes:
; * removed: 1
; * added: 5
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 2
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
                                                (<change>
                                                   ()
                                                   l)
                                                (output " ")
                                                (<change>
                                                   (output (car l))
                                                   (output "..."))
                                                (<change>
                                                   (output "...")
                                                   (output (car l))))
                                             (begin
                                                (<change>
                                                   (output " ")
                                                   ())
                                                (output (car l))
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
                                      (<change>
                                         ()
                                         (cdr l))
                                      (if (eq? (cdr l) r)
                                         (<change>
                                            (begin
                                               (set! last (cons (car l) ()))
                                               last)
                                            (cons (car l) (aux (cdr l))))
                                         (<change>
                                            (cons (car l) (aux (cdr l)))
                                            (begin
                                               (set! last (cons (car l) ()))
                                               last))))))
                         (<change>
                            ()
                            set-cdr!)
                         (let ((first (aux r)))
                            (set-cdr! last first)
                            first))))
         (right-rotate (lambda (r)
                         (letrec ((iter (lambda (l)
                                          (if (eq? (cdr l) r) l (iter (cdr l))))))
                            (iter r))))
         (Josephus (lambda (r n)
                     (letrec ((remove-nth! (lambda (l n)
                                             (<change>
                                                (if (<= n 2)
                                                   (begin
                                                      (set-cdr! l (cddr l))
                                                      (cdr l))
                                                   (remove-nth! (cdr l) (- n 1)))
                                                ((lambda (x) x) (if (<= n 2) (begin (set-cdr! l (cddr l)) (cdr l)) (remove-nth! (cdr l) (- n 1)))))))
                              (iter (lambda (l)
                                      (print-ring l)
                                      (if (eq? l (cdr l))
                                         (car l)
                                         (iter (remove-nth! l n))))))
                        (if (= n 1)
                           (car (right-rotate r))
                           (iter (copy-ring r))))))
         (ring (make-ring 5)))
   (Josephus ring 5)
   (<change>
      (print-ring ring)
      ((lambda (x) x) (print-ring ring)))
   (<change>
      ()
      __toplevel_cons)
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