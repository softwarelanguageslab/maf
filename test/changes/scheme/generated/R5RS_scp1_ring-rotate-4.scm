; Changes:
; * removed: 2
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 2
; * calls to id fun: 1
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
                                                   ((lambda (x) x) (output " "))
                                                   (output "..."))))
                                          #f))))
                          (<change>
                             (aux r)
                             ())
                          #t)))
         (right-rotate (lambda (r)
                         (letrec ((iter (lambda (l)
                                          (<change>
                                             ()
                                             (if (eq? (cdr l) r) (iter (cdr l)) l))
                                          (if (eq? (cdr l) r) l (iter (cdr l))))))
                            (iter r))))
         (r (make-ring 3)))
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