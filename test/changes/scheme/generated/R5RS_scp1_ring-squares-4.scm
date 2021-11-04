; Changes:
; * removed: 1
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((result ())
         (output (lambda (i)
                   (<change>
                      ()
                      cons)
                   (set! result (cons i result))))
         (kw-lijst (lambda (lst)
                     (<change>
                        ()
                        (display (* (car l) (car l))))
                     (letrec ((loop (lambda (l)
                                      (let ((rest (cdr l))
                                            (n (list (* (car l) (car l)))))
                                         (set-cdr! l n)
                                         (<change>
                                            (set-cdr! n rest)
                                            ((lambda (x) x) (set-cdr! n rest)))
                                         (if (not (eq? rest lst)) (loop rest) #f)))))
                        (<change>
                           (loop lst)
                           ())
                        lst)))
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
         (last-cons (cons 3 ()))
         (test-lst (cons 1 (cons 4 last-cons))))
   (set-cdr! last-cons test-lst)
   (print-ring (kw-lijst test-lst))
   (equal?
      result
      (__toplevel_cons
         "..."
         (__toplevel_cons
            9
            (__toplevel_cons
               " "
               (__toplevel_cons
                  3
                  (__toplevel_cons
                     " "
                     (__toplevel_cons
                        16
                        (__toplevel_cons
                           " "
                           (__toplevel_cons
                              4
                              (__toplevel_cons
                                 " "
                                 (__toplevel_cons 1 (__toplevel_cons " " (__toplevel_cons 1 (__toplevel_cons " " ())))))))))))))))