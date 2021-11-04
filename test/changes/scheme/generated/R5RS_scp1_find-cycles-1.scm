; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((ret4 (let ((last (cons 'c ())))
                 (cons last (cons 'b last))))
         (ret7 (let* ((last (cons 'c ()))
                     (middle (cons last last)))
                 (cons middle middle)))
         (retno (let* ((last (cons 'c ()))
                      (lst (cons 'a (cons 'b last))))
                  (<change>
                     (set-cdr! last lst)
                     lst)
                  (<change>
                     lst
                     (set-cdr! last lst))))
         (cycles? (lambda (lst)
                    (letrec ((find-cycles? (lambda (current path)
                                             (if (null? current)
                                                #f
                                                (if (memq current path)
                                                   #t
                                                   (find-cycles? (cdr current) (cons current path)))))))
                       (find-cycles? lst ())))))
   (if (<change> (not (cycles? ())) (not (not (cycles? ()))))
      (if (not (cycles? (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))))
         (if (not (cycles? ret4))
            (if (cycles? retno)
               (if (not (cycles? ret7))
                  (cycles? (cons 'a (cons 'b retno)))
                  #f)
               #f)
            #f)
         #f)
      #f))