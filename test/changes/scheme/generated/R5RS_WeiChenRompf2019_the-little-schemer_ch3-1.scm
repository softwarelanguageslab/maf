; Changes:
; * removed: 1
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 3
(letrec ((rember (lambda (a lat)
                   (<change>
                      (if (null? lat)
                         ()
                         (if (eq? (car lat) a)
                            (cdr lat)
                            (cons (car lat) (rember a (cdr lat)))))
                      ((lambda (x) x)
                         (if (null? lat)
                            ()
                            (if (eq? (car lat) a)
                               (cdr lat)
                               (cons (car lat) (rember a (cdr lat))))))))))
   (rember 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'a (__toplevel_cons 'd ())))))
   (letrec ((first (lambda (l)
                     (<change>
                        (if (null? l)
                           ()
                           (cons (car (car l)) (first (cdr l))))
                        ((lambda (x) x) (if (null? l) () (cons (car (car l)) (first (cdr l)))))))))
      (first
         (__toplevel_cons
            (__toplevel_cons 'a (__toplevel_cons 'b ()))
            (__toplevel_cons
               (__toplevel_cons 'c (__toplevel_cons 'd ()))
               (__toplevel_cons (__toplevel_cons 'e (__toplevel_cons 'f ())) ()))))
      (letrec ((insertR (lambda (new old lat)
                          (if (null? lat)
                             ()
                             (if (eq? (car lat) old)
                                (cons old (cons new (cdr lat)))
                                (cons (car lat) (insertR new old (cdr lat))))))))
         (insertR
            'z
            'b
            (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd ())))))
         (<change>
            ()
            (__toplevel_cons 'c (__toplevel_cons 'b (__toplevel_cons 'd ()))))
         (letrec ((insertL (lambda (new old lat)
                             (if (null? lat)
                                ()
                                (if (eq? (car lat) old)
                                   (cons new lat)
                                   (cons (car lat) (insertL new old (cdr lat))))))))
            (insertL
               'z
               'b
               (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd ())))))
            (letrec ((subst (lambda (new old lat)
                              (if (null? lat)
                                 ()
                                 (if (eq? (car lat) old)
                                    (<change>
                                       (cons new (cdr lat))
                                       (cons (car lat) (subst new old (cdr lat))))
                                    (<change>
                                       (cons (car lat) (subst new old (cdr lat)))
                                       (cons new (cdr lat))))))))
               (subst
                  'z
                  'b
                  (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd ())))))
               (letrec ((subst2 (lambda (new o1 o2 lat)
                                  (if (null? lat)
                                     ()
                                     (if (let ((__or_res (eq? (car lat) o1))) (<change> (if __or_res __or_res (eq? (car lat) o2)) ((lambda (x) x) (if __or_res __or_res (eq? (car lat) o2)))))
                                        (cons new (cdr lat))
                                        (cons (car lat) (subst2 new o1 o2 (cdr lat))))))))
                  (<change>
                     (subst2
                        'z
                        'c
                        'b
                        (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd ())))))
                     ())
                  (subst2
                     'z
                     'c
                     'b
                     (__toplevel_cons 'a (__toplevel_cons 'c (__toplevel_cons 'b (__toplevel_cons 'd ())))))
                  (letrec ((multirember (lambda (a lat)
                                          (if (null? lat)
                                             ()
                                             (if (eq? (car lat) a)
                                                (multirember a (cdr lat))
                                                (cons (car lat) (multirember a (cdr lat))))))))
                     (multirember
                        'a
                        (__toplevel_cons
                           'x
                           (__toplevel_cons 'y (__toplevel_cons 'a (__toplevel_cons 'z (__toplevel_cons 'a ()))))))
                     (letrec ((multiinsertR (lambda (new old lat)
                                              (if (null? lat)
                                                 ()
                                                 (if (eq? (car lat) old)
                                                    (cons old (cons new (multiinsertR new old (cdr lat))))
                                                    (cons (car lat) (multiinsertR new old (cdr lat))))))))
                        (multiinsertR
                           'a
                           'b
                           (__toplevel_cons
                              'x
                              (__toplevel_cons 'b (__toplevel_cons 'y (__toplevel_cons 'b (__toplevel_cons 'z ()))))))
                        (letrec ((multiinsertL (lambda (new old lat)
                                                 (if (null? lat)
                                                    ()
                                                    (if (eq? (car lat) old)
                                                       (cons new (cons old (multiinsertL new old (cdr lat))))
                                                       (cons (car lat) (multiinsertL new old (cdr lat))))))))
                           (multiinsertL
                              'a
                              'b
                              (__toplevel_cons
                                 'x
                                 (__toplevel_cons 'b (__toplevel_cons 'y (__toplevel_cons 'b (__toplevel_cons 'z ()))))))
                           (letrec ((multisubst (lambda (new old lat)
                                                  (if (null? lat)
                                                     ()
                                                     (if (eq? (car lat) old)
                                                        (cons new (multisubst new old (cdr lat)))
                                                        (cons (car lat) (multisubst new old (cdr lat))))))))
                              (multisubst
                                 'a
                                 'b
                                 (__toplevel_cons
                                    'x
                                    (__toplevel_cons 'b (__toplevel_cons 'y (__toplevel_cons 'b (__toplevel_cons 'z ()))))))))))))))))