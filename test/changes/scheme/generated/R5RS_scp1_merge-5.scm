; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 2
(letrec ((first-el (lambda (best)
                     (<change>
                        ()
                        not)
                     (if (not (null? best)) (caar best) #f)))
         (smaller? (lambda (el1 el2)
                     (<change>
                        (string<? (symbol->string el1) (symbol->string el2))
                        ((lambda (x) x) (string<? (symbol->string el1) (symbol->string el2))))))
         (same? (lambda (el1 el2)
                  (<change>
                     (equal? el1 el2)
                     ((lambda (x) x) (equal? el1 el2)))))
         (merge (lambda (best1 best2)
                  (letrec ((merge-in (lambda (curr1 curr2 prev)
                                       (if (null? curr1)
                                          (set-cdr! prev curr2)
                                          (if (<change> (null? curr2) (not (null? curr2)))
                                             (set-cdr! prev curr1)
                                             (if (same? (first-el curr1) (first-el curr2))
                                                (begin
                                                   (set-cdr! prev curr1)
                                                   (merge-in (cdr curr1) (cdr curr2) curr1))
                                                (if (smaller? (first-el curr1) (first-el curr2))
                                                   (<change>
                                                      (begin
                                                         (set-cdr! prev curr1)
                                                         (merge-in (cdr curr1) curr2 curr1))
                                                      (begin
                                                         (set-cdr! prev curr2)
                                                         (merge-in curr1 (cdr curr2) curr2)))
                                                   (<change>
                                                      (begin
                                                         (set-cdr! prev curr2)
                                                         (merge-in curr1 (cdr curr2) curr2))
                                                      (begin
                                                         (merge-in (cdr curr1) curr2 curr1)
                                                         (set-cdr! prev curr1))))))))))
                     (let* ((result (if (smaller? (first-el best1) (first-el best2))
                                      best1
                                      best2))
                            (curr1 (if (eq? result best1) (cdr best1) best1))
                            (curr2 (if (eq? result best2) (cdr best2) best2)))
                        (merge-in curr1 curr2 result)
                        result))))
         (best1 (__toplevel_cons
                  (__toplevel_cons
                     'ann
                     (__toplevel_cons
                        (__toplevel_cons
                           'meiboomstraat
                           (__toplevel_cons 12 (__toplevel_cons 1820 (__toplevel_cons 'Eppegem ()))))
                        ()))
                  (__toplevel_cons
                     (__toplevel_cons
                        'bert
                        (__toplevel_cons
                           (__toplevel_cons
                              'populierendreef
                              (__toplevel_cons 7 (__toplevel_cons 1050 (__toplevel_cons 'Brussel ()))))
                           ()))
                     (__toplevel_cons
                        (__toplevel_cons
                           'kurt
                           (__toplevel_cons
                              (__toplevel_cons
                                 'Mechelsesteenweg
                                 (__toplevel_cons 50 (__toplevel_cons 1800 (__toplevel_cons 'Vilvoorde ()))))
                              ()))
                        ()))))
         (best2 (__toplevel_cons
                  (__toplevel_cons
                     'bert
                     (__toplevel_cons
                        (__toplevel_cons
                           'populierendreef
                           (__toplevel_cons 7 (__toplevel_cons 1050 (__toplevel_cons 'Brussel ()))))
                        ()))
                  (__toplevel_cons
                     (__toplevel_cons
                        'jan
                        (__toplevel_cons
                           (__toplevel_cons 'eikestraat (__toplevel_cons 1 (__toplevel_cons 9000 (__toplevel_cons 'Gent ()))))
                           ()))
                     (__toplevel_cons
                        (__toplevel_cons
                           'sofie
                           (__toplevel_cons
                              (__toplevel_cons
                                 'boerendreef
                                 (__toplevel_cons 5 (__toplevel_cons 2800 (__toplevel_cons 'Mechelen ()))))
                              ()))
                        ())))))
   (equal?
      (merge best1 best2)
      (__toplevel_cons
         (__toplevel_cons
            'ann
            (__toplevel_cons
               (__toplevel_cons
                  'meiboomstraat
                  (__toplevel_cons 12 (__toplevel_cons 1820 (__toplevel_cons 'Eppegem ()))))
               ()))
         (__toplevel_cons
            (__toplevel_cons
               'bert
               (__toplevel_cons
                  (__toplevel_cons
                     'populierendreef
                     (__toplevel_cons 7 (__toplevel_cons 1050 (__toplevel_cons 'Brussel ()))))
                  ()))
            (__toplevel_cons
               (__toplevel_cons
                  'jan
                  (__toplevel_cons
                     (__toplevel_cons 'eikestraat (__toplevel_cons 1 (__toplevel_cons 9000 (__toplevel_cons 'Gent ()))))
                     ()))
               (__toplevel_cons
                  (__toplevel_cons
                     'kurt
                     (__toplevel_cons
                        (__toplevel_cons
                           'Mechelsesteenweg
                           (__toplevel_cons 50 (__toplevel_cons 1800 (__toplevel_cons 'Vilvoorde ()))))
                        ()))
                  (__toplevel_cons
                     (__toplevel_cons
                        'sofie
                        (__toplevel_cons
                           (__toplevel_cons
                              'boerendreef
                              (__toplevel_cons 5 (__toplevel_cons 2800 (__toplevel_cons 'Mechelen ()))))
                           ()))
                     ())))))))