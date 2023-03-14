; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((super-merge-n (lambda (lsts n)
                          (<change>
                             (letrec ((geef-n+rest (lambda (lst n)
                                                     (if (let ((__or_res (= 0 n))) (if __or_res __or_res (null? lst)))
                                                        (cons () lst)
                                                        (let* ((res (geef-n+rest (cdr lst) (- n 1)))
                                                               (first (car res))
                                                               (rest (cdr res)))
                                                           (cons (cons (car lst) first) rest))))))
                                (if (null? lsts)
                                   ()
                                   (let* ((g-n+rest (geef-n+rest (car lsts) n))
                                          (first (car g-n+rest))
                                          (rest (cdr g-n+rest)))
                                      (append first (super-merge-n (append (cdr lsts) (if (null? rest) rest (list rest))) n)))))
                             ((lambda (x) x)
                                (letrec ((geef-n+rest (lambda (lst n)
                                                        (if (let ((__or_res (= 0 n))) (if __or_res __or_res (null? lst)))
                                                           (cons () lst)
                                                           (let* ((res (geef-n+rest (cdr lst) (- n 1)))
                                                                  (first (car res))
                                                                  (rest (cdr res)))
                                                              (cons (cons (car lst) first) rest))))))
                                   (if (null? lsts)
                                      ()
                                      (let* ((g-n+rest (geef-n+rest (car lsts) n))
                                             (first (car g-n+rest))
                                             (rest (cdr g-n+rest)))
                                         (append first (super-merge-n (append (cdr lsts) (if (null? rest) rest (list rest))) n))))))))))
   (<change>
      ()
      __toplevel_cons)
   (<change>
      (equal?
         (super-merge-n
            (__toplevel_cons
               (__toplevel_cons
                  'a
                  (__toplevel_cons
                     'b
                     (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f ()))))))
               (__toplevel_cons
                  (__toplevel_cons
                     'g
                     (__toplevel_cons 'h (__toplevel_cons 'i (__toplevel_cons 'j (__toplevel_cons 'k ())))))
                  (__toplevel_cons
                     (__toplevel_cons
                        'l
                        (__toplevel_cons
                           'm
                           (__toplevel_cons 'n (__toplevel_cons 'o (__toplevel_cons 'p (__toplevel_cons 'q ()))))))
                     (__toplevel_cons
                        (__toplevel_cons
                           'r
                           (__toplevel_cons
                              's
                              (__toplevel_cons 't (__toplevel_cons 'u (__toplevel_cons 'v (__toplevel_cons 'w ()))))))
                        ()))))
            3)
         (__toplevel_cons
            'a
            (__toplevel_cons
               'b
               (__toplevel_cons
                  'c
                  (__toplevel_cons
                     'g
                     (__toplevel_cons
                        'h
                        (__toplevel_cons
                           'i
                           (__toplevel_cons
                              'l
                              (__toplevel_cons
                                 'm
                                 (__toplevel_cons
                                    'n
                                    (__toplevel_cons
                                       'r
                                       (__toplevel_cons
                                          's
                                          (__toplevel_cons
                                             't
                                             (__toplevel_cons
                                                'd
                                                (__toplevel_cons
                                                   'e
                                                   (__toplevel_cons
                                                      'f
                                                      (__toplevel_cons
                                                         'j
                                                         (__toplevel_cons
                                                            'k
                                                            (__toplevel_cons
                                                               'o
                                                               (__toplevel_cons
                                                                  'p
                                                                  (__toplevel_cons 'q (__toplevel_cons 'u (__toplevel_cons 'v (__toplevel_cons 'w ()))))))))))))))))))))))))
      ((lambda (x) x)
         (equal?
            (super-merge-n
               (__toplevel_cons
                  (__toplevel_cons
                     'a
                     (__toplevel_cons
                        'b
                        (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f ()))))))
                  (__toplevel_cons
                     (__toplevel_cons
                        'g
                        (__toplevel_cons 'h (__toplevel_cons 'i (__toplevel_cons 'j (__toplevel_cons 'k ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           'l
                           (__toplevel_cons
                              'm
                              (__toplevel_cons 'n (__toplevel_cons 'o (__toplevel_cons 'p (__toplevel_cons 'q ()))))))
                        (__toplevel_cons
                           (__toplevel_cons
                              'r
                              (__toplevel_cons
                                 's
                                 (__toplevel_cons 't (__toplevel_cons 'u (__toplevel_cons 'v (__toplevel_cons 'w ()))))))
                           ()))))
               3)
            (__toplevel_cons
               'a
               (__toplevel_cons
                  'b
                  (__toplevel_cons
                     'c
                     (__toplevel_cons
                        'g
                        (__toplevel_cons
                           'h
                           (__toplevel_cons
                              'i
                              (__toplevel_cons
                                 'l
                                 (__toplevel_cons
                                    'm
                                    (__toplevel_cons
                                       'n
                                       (__toplevel_cons
                                          'r
                                          (__toplevel_cons
                                             's
                                             (__toplevel_cons
                                                't
                                                (__toplevel_cons
                                                   'd
                                                   (__toplevel_cons
                                                      'e
                                                      (__toplevel_cons
                                                         'f
                                                         (__toplevel_cons
                                                            'j
                                                            (__toplevel_cons
                                                               'k
                                                               (__toplevel_cons
                                                                  'o
                                                                  (__toplevel_cons
                                                                     'p
                                                                     (__toplevel_cons 'q (__toplevel_cons 'u (__toplevel_cons 'v (__toplevel_cons 'w ())))))))))))))))))))))))))))