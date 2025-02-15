; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 2
; * calls to id fun: 0
(letrec ((rember (lambda (a lat)
                   (if (null? lat)
                      ()
                      (if (eq? (car lat) a)
                         (cdr lat)
                         (cons (car lat) (rember a (cdr lat))))))))
   (rember 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'a (__toplevel_cons 'd ())))))
   (letrec ((first (lambda (l)
                     (if (null? l)
                        (<change>
                           ()
                           (cons (car (car l)) (first (cdr l))))
                        (<change>
                           (cons (car (car l)) (first (cdr l)))
                           ())))))
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
                                    (cons new (cdr lat))
                                    (cons (car lat) (subst new old (cdr lat))))))))
               (subst
                  'z
                  'b
                  (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd ())))))
               (letrec ((subst2 (lambda (new o1 o2 lat)
                                  (if (null? lat)
                                     ()
                                     (if (let ((__or_res (eq? (car lat) o1))) (if __or_res __or_res (eq? (car lat) o2)))
                                        (<change>
                                           (cons new (cdr lat))
                                           (cons (car lat) (subst2 new o1 o2 (cdr lat))))
                                        (<change>
                                           (cons (car lat) (subst2 new o1 o2 (cdr lat)))
                                           (cons new (cdr lat))))))))
                  (subst2
                     'z
                     'c
                     'b
                     (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd ())))))
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
                     (<change>
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
                                       (__toplevel_cons 'b (__toplevel_cons 'y (__toplevel_cons 'b (__toplevel_cons 'z ()))))))))))
                     (<change>
                        (letrec ((llll (lambda (new old lat)
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
                                       (__toplevel_cons 'b (__toplevel_cons 'y (__toplevel_cons 'b (__toplevel_cons 'z ())))))))))
                        (multirember
                           'a
                           (__toplevel_cons
                              'x
                              (__toplevel_cons 'y (__toplevel_cons 'a (__toplevel_cons 'z (__toplevel_cons 'a ()))))))))))))))