; Changes:
; * removed: 0
; * added: 1
; * swaps: 2
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 3
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
                               ring)))))
         (print-ring (lambda (r)
                       (<change>
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
                             #t)
                          ((lambda (x) x)
                             (letrec ((aux (lambda (l)
                                             (if (not (null? l))
                                                (<change>
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
                                                (<change>
                                                   #f
                                                   (if (eq? (cdr l) r)
                                                      (begin
                                                         (output " ")
                                                         (output "...")
                                                         l
                                                         (output (car l)))
                                                      (begin
                                                         (output " ")
                                                         (output (car l))
                                                         ((lambda (x) x) (aux (cdr l))))))))))
                                (aux r)
                                #t)))))
         (right-rotate (lambda (r)
                         (letrec ((iter (lambda (l)
                                          (if (eq? (cdr l) r) l (iter (cdr l))))))
                            (iter r))))
         (r (make-ring 3)))
   (<change>
      (print-ring (right-rotate r))
      (equal?
         result
         (__toplevel_cons
            "..."
            (__toplevel_cons
               1
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     2
                     (__toplevel_cons
                        " "
                        (__toplevel_cons 3 (__toplevel_cons " " (__toplevel_cons 0 (__toplevel_cons " " ())))))))))))
   (<change>
      (equal?
         result
         (__toplevel_cons
            "..."
            (__toplevel_cons
               1
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     2
                     (__toplevel_cons
                        " "
                        (__toplevel_cons 3 (__toplevel_cons " " (__toplevel_cons 0 (__toplevel_cons " " ()))))))))))
      (print-ring (right-rotate r))))