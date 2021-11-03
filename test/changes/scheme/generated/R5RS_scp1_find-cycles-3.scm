; Changes:
; * removed: 1
; * added: 1
; * swaps: 0
; * negated predicates: 1
(letrec ((ret4 (let ((last (cons 'c ())))
                 (cons last (cons 'b last))))
         (ret7 (let* ((last (cons 'c ()))
                     (middle (cons last last)))
                 (cons middle middle)))
         (retno (let* ((last (cons 'c ()))
                      (lst (cons 'a (cons 'b last))))
                  (<change>
                     (set-cdr! last lst)
                     ())
                  (<change>
                     ()
                     lst)
                  lst))
         (cycles? (lambda (lst)
                    (letrec ((find-cycles? (lambda (current path)
                                             (if (null? current)
                                                #f
                                                (if (memq current path)
                                                   #t
                                                   (find-cycles? (cdr current) (cons current path)))))))
                       (find-cycles? lst ())))))
   (if (not (cycles? ()))
      (if (not (cycles? (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))))
         (if (<change> (not (cycles? ret4)) (not (not (cycles? ret4))))
            (if (cycles? retno)
               (if (not (cycles? ret7))
                  (cycles? (cons 'a (cons 'b retno)))
                  #f)
               #f)
            #f)
         #f)
      #f))