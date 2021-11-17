; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((quick-sort (lambda (a-list)
                       (<change>
                          ()
                          <=)
                       (letrec ((rearrange (lambda (pivot some-list)
                                             (letrec ((rearrange-iter (lambda (rest result)
                                                                        (if (null? rest)
                                                                           result
                                                                           (if (<= (car rest) pivot)
                                                                              (rearrange-iter (cdr rest) (cons (cons (car rest) (car result)) (cdr result)))
                                                                              (rearrange-iter (cdr rest) (cons (car result) (cons (car rest) (cdr result)))))))))
                                                (rearrange-iter some-list (cons () ()))))))
                          (if (<= (length a-list) 1)
                             a-list
                             (let* ((pivot (car a-list))
                                    (sub-lists (rearrange pivot (cdr a-list))))
                                (append (quick-sort (car sub-lists)) (append (list pivot) (quick-sort (cdr sub-lists))))))))))
   (equal?
      (quick-sort
         (__toplevel_cons
            9
            (__toplevel_cons
               8
               (__toplevel_cons
                  7
                  (__toplevel_cons
                     6
                     (__toplevel_cons
                        5
                        (__toplevel_cons
                           4
                           (__toplevel_cons
                              3
                              (__toplevel_cons 2 (__toplevel_cons 1 (__toplevel_cons 0 (__toplevel_cons 9 ()))))))))))))
      (__toplevel_cons
         0
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
                           (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 (__toplevel_cons 9 ())))))))))))))