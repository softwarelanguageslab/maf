; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((find-last (lambda (lijst)
                      (if (null? lijst)
                         (error "find-last -- lijst heeft geen laatste element")
                         (let ((next (cdr lijst)))
                            (if (null? next) lijst (find-last next))))))
         (flatten! (lambda (lijst)
                     (if (null? lijst)
                        ()
                        (let* ((sublist (car lijst))
                               (restlist (flatten! (cdr lijst))))
                           (if (null? sublist)
                              restlist
                              (let ((last (find-last sublist)))
                                 (set-cdr! last restlist)
                                 sublist))))))
         (atom? (lambda (x)
                  (not (pair? x))))
         (flatten2! (lambda (lijst)
                      (let ((hulpcel (cons 'dummy lijst)))
                         (letrec ((flatten-aux! (lambda (prev current)
                                                  (if (null? current)
                                                     (cdr hulpcel)
                                                     (if (null? (car current))
                                                        (begin
                                                           (set-cdr! prev (cdr current))
                                                           (flatten-aux! prev (cdr current)))
                                                        (if (pair? (car current))
                                                           (<change>
                                                              (begin
                                                                 (set-cdr! prev (flatten2! (car current)))
                                                                 (flatten-aux! (find-last prev) (cdr current)))
                                                              (if (null? (cdr prev))
                                                                 (begin
                                                                    (set-cdr! prev current)
                                                                    (flatten-aux! (cdr prev) (cdr current)))
                                                                 (if (atom? (car current))
                                                                    (flatten-aux! (cdr prev) (cdr current))
                                                                    #f)))
                                                           (<change>
                                                              (if (null? (cdr prev))
                                                                 (begin
                                                                    (set-cdr! prev current)
                                                                    (flatten-aux! (cdr prev) (cdr current)))
                                                                 (if (atom? (car current))
                                                                    (flatten-aux! (cdr prev) (cdr current))
                                                                    #f))
                                                              (begin
                                                                 (flatten-aux! (find-last prev) (cdr current))
                                                                 ((lambda (x) x) (set-cdr! prev (flatten2! (car current))))))))))))
                            (flatten-aux! hulpcel lijst)
                            (cdr hulpcel)))))
         (res (if (equal? (flatten! (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
                (if (equal? (flatten! (__toplevel_cons () (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons () (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
                   (if (equal? (flatten2! (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons (__toplevel_cons 2 (__toplevel_cons 3 ())) (__toplevel_cons 4 ()))) (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
                      (if (equal? (flatten2! (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
                         (if (equal? (flatten2! (__toplevel_cons () (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons () (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
                            (equal?
                               (flatten2!
                                  (__toplevel_cons
                                     1
                                     (__toplevel_cons
                                        2
                                        (__toplevel_cons
                                           (__toplevel_cons
                                              3
                                              (__toplevel_cons
                                                 (__toplevel_cons 4 (__toplevel_cons 5 ()))
                                                 (__toplevel_cons
                                                    6
                                                    (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) (__toplevel_cons 9 ())))))
                                           (__toplevel_cons 10 ())))))
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
                                              (__toplevel_cons
                                                 6
                                                 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 (__toplevel_cons 10 ())))))))))))
                            #f)
                         #f)
                      #f)
                   #f)
                #f)))
   res)