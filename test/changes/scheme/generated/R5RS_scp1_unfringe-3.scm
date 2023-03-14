; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((unfringe-1 (lambda (l)
                       (<change>
                          ()
                          ())
                       (if (null? l)
                          ()
                          (if (null? (cdr l))
                             (list (car l))
                             (list (car l) (unfringe-1 (cdr l)))))))
         (unfringe-2 (lambda (l)
                       (<change>
                          ()
                          l)
                       (letrec ((pair (lambda (l)
                                        (if (null? l)
                                           ()
                                           (if (null? (cdr l))
                                              (list l)
                                              (cons (list (car l) (cadr l)) (pair (cddr l))))))))
                          ((letrec ((loop (lambda (l)
                                           (if (let ((__or_res (null? l))) (if __or_res __or_res (null? (cdr l))))
                                              l
                                              (loop (pair l))))))
                             loop)
                             l)))))
   (if (equal? (unfringe-1 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ())))))))))) (__toplevel_cons 1 (__toplevel_cons (__toplevel_cons 2 (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons (__toplevel_cons 4 (__toplevel_cons (__toplevel_cons 5 (__toplevel_cons (__toplevel_cons 6 (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons (__toplevel_cons 8 (__toplevel_cons (__toplevel_cons 9 ()) ())) ())) ())) ())) ())) ())) ())) ())))
      (equal?
         (unfringe-2
            (__toplevel_cons
               1
               (__toplevel_cons
                  2
                  (__toplevel_cons
                     3
                     (__toplevel_cons
                        4
                        (__toplevel_cons
                           5
                           (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ()))))))))))
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons 1 (__toplevel_cons 2 ()))
                     (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ()))
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons 5 (__toplevel_cons 6 ()))
                        (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))
                     ()))
               (__toplevel_cons (__toplevel_cons (__toplevel_cons (__toplevel_cons 9 ()) ()) ()) ()))
            ()))
      #f))