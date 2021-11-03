; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 3
; * calls to id fun: 2
(letrec ((find-last (lambda (lijst)
                      (if (<change> (null? lijst) (not (null? lijst)))
                         (error "find-last -- lijst heeft geen laatste element")
                         (let ((next (cdr lijst)))
                            (if (null? next) lijst (find-last next))))))
         (flatten! (lambda (lijst)
                     (if (null? lijst)
                        ()
                        (let* ((sublist (car lijst))
                               (restlist (flatten! (cdr lijst))))
                           (if (null? sublist)
                              (<change>
                                 restlist
                                 (let ((last (find-last sublist)))
                                    (set-cdr! last restlist)
                                    set-cdr!
                                    sublist))
                              (<change>
                                 (let ((last (find-last sublist)))
                                    (set-cdr! last restlist)
                                    sublist)
                                 restlist))))))
         (atom? (lambda (x)
                  (not (pair? x))))
         (flatten2! (lambda (lijst)
                      (let ((hulpcel (cons 'dummy lijst)))
                         (<change>
                            ()
                            current)
                         (letrec ((flatten-aux! (lambda (prev current)
                                                  (if (null? current)
                                                     (cdr hulpcel)
                                                     (if (null? (car current))
                                                        (begin
                                                           (set-cdr! prev (cdr current))
                                                           (<change>
                                                              (flatten-aux! prev (cdr current))
                                                              ((lambda (x) x) (flatten-aux! prev (cdr current)))))
                                                        (if (pair? (car current))
                                                           (begin
                                                              (set-cdr! prev (flatten2! (car current)))
                                                              (flatten-aux! (find-last prev) (cdr current)))
                                                           (if (null? (cdr prev))
                                                              (<change>
                                                                 (begin
                                                                    (set-cdr! prev current)
                                                                    (flatten-aux! (cdr prev) (cdr current)))
                                                                 (if (atom? (car current))
                                                                    #f
                                                                    (flatten-aux! (cdr prev) (cdr current))))
                                                              (<change>
                                                                 (if (atom? (car current))
                                                                    (flatten-aux! (cdr prev) (cdr current))
                                                                    #f)
                                                                 (begin
                                                                    (set-cdr! prev current)
                                                                    ((lambda (x) x) (flatten-aux! (cdr prev) (cdr current))))))))))))
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