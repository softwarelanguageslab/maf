; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((result ())
         (output (lambda (i)
                   (<change>
                      ()
                      result)
                   (<change>
                      (set! result (cons i result))
                      ((lambda (x) x) (set! result (cons i result))))))
         (make-ring (lambda (n)
                      (<change>
                         (let ((last (cons 0 ())))
                            (letrec ((build-list (lambda (n)
                                                   (if (= n 0) last (cons n (build-list (- n 1)))))))
                               (let ((ring (build-list n)))
                                  (set-cdr! last ring)
                                  ring)))
                         ((lambda (x) x)
                            (let ((last (cons 0 ())))
                               (letrec ((build-list (lambda (n)
                                                      (if (= n 0) last (cons n (build-list (- n 1)))))))
                                  (let ((ring (build-list n)))
                                     (set-cdr! last ring)
                                     ring)))))))
         (print-ring (lambda (r)
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
                          #t)))
         (right-rotate (lambda (r)
                         (letrec ((iter (lambda (l)
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