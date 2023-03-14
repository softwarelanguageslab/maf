; Changes:
; * removed: 2
; * added: 2
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 4
(letrec ((atom? (lambda (x)
                  (if (not (pair? x)) (not (null? x)) #f)))
         (rember-f (lambda (test?)
                     (lambda (a l)
                        (if (null? l)
                           ()
                           (if (test? a (car l))
                              (cdr l)
                              (cons (car l) (rember-f test? a (cdr l))))))))
         (eq?-c (lambda (a)
                  (lambda (x)
                     (eq? x a))))
         (eq?-salad (eq?-c 'salad))
         (rember-eq? (rember-f eq?))
         (rember-equal? (rember-f equal?))
         (insertL-f (lambda (test?)
                      (lambda (new old l)
                         (if (null? l)
                            ()
                            (if (test? (car l) old)
                               (cons new (cons old (cdr l)))
                               (cons (car l) ((insertL-f test?) new old (cdr l))))))))
         (insertR-f (lambda (test?)
                      (lambda (new old l)
                         (if (null? l)
                            ()
                            (if (test? (car l) old)
                               (cons old (cons new (cdr l)))
                               (cons (car l) ((insertR-f test?) new old (cdr l))))))))
         (insert-left (lambda (new old l)
                        (cons new (cons old l))))
         (insert-right (lambda (new old l)
                         (cons old (cons new l))))
         (insert-g (lambda (test?)
                     (<change>
                        (lambda (insert)
                           (lambda (new old l)
                              (if (null? l)
                                 ()
                                 (if (test? (car l) old)
                                    (insert new old (cdr l))
                                    (cons (car l) (((insert-g test?) insert) new old (cdr l)))))))
                        ((lambda (x) x)
                           (lambda (insert)
                              (lambda (new old l)
                                 (if (null? l)
                                    ()
                                    (if (test? (car l) old)
                                       (insert new old (cdr l))
                                       (cons (car l) (((insert-g test?) insert) new old (cdr l))))))))))))
   (((insert-g equal?) insert-left)
      'a
      'b
      (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'b ())))))
   (((insert-g equal?) insert-right)
      'a
      'b
      (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'b ())))))
   (letrec ((seqL (lambda (new old l)
                    (<change>
                       (cons new (cons old l))
                       ((lambda (x) x) (cons new (cons old l))))))
            (seqR (lambda (new old l)
                    (cons old (cons new l))))
            (insertR (insert-g seqR))
            (insertL (insert-g (lambda (new old l) (cons new (cons old l)))))
            (seqS (lambda (new old l)
                    (cons new l)))
            (subst (insert-g seqS))
            (seqrem (lambda (new old l)
                      l))
            (yyy (lambda (a l)
                   (<change>
                      ()
                      a)
                   (<change>
                      ((insert-g seqrem) #f a l)
                      ((lambda (x) x) ((insert-g seqrem) #f a l)))))
            (^ (lambda (n m)
                 (expt n m)))
            (operator (lambda (aexp)
                        (car aexp)))
            (first-sub-exp (lambda (aexp)
                             (car (cdr aexp))))
            (second-sub-exp (lambda (aexp)
                              (car (cdr (cdr aexp)))))
            (atom-to-function (lambda (x)
                                (if (<change> (eq? x '+) (not (eq? x '+)))
                                   +
                                   (if (eq? x '*) * ^))))
            (value (lambda (nexp)
                     (if (atom? nexp)
                        nexp
                        ((atom-to-function (operator nexp)) (value (first-sub-exp nexp)) (value (second-sub-exp nexp))))))
            (multirember-f (lambda (test?)
                             (<change>
                                (lambda (a lat)
                                   (if (null? lat)
                                      ()
                                      (if (test? (car lat) a)
                                         ((multirember-f test?) a (cdr lat))
                                         (cons (car lat) ((multirember-f test?) a (cdr lat))))))
                                ((lambda (x) x)
                                   (lambda (a lat)
                                      (if (null? lat)
                                         ()
                                         (if (test? (car lat) a)
                                            ((multirember-f test?) a (cdr lat))
                                            (cons (car lat) ((multirember-f test?) a (cdr lat))))))))))
            (multirember-eq? (multirember-f eq?))
            (multiremberT (lambda (test? lat)
                            (if (null? lat)
                               ()
                               (if (test? (car lat))
                                  (multiremberT test? (cdr lat))
                                  (cons (car lat) (multiremberT test? (cdr lat))))))))
      (multiremberT
         (lambda (x)
            (eq? x 'a))
         (__toplevel_cons
            'b
            (__toplevel_cons
               'c
               (__toplevel_cons 'd (__toplevel_cons 'a (__toplevel_cons 'x (__toplevel_cons 'a ())))))))
      (letrec ((multirember&co (lambda (a lat col)
                                 (if (null? lat)
                                    (col () ())
                                    (if (eq? (car lat) a)
                                       (multirember&co a (cdr lat) (lambda (newlat seen) (col newlat (cons (car lat) seen))))
                                       (multirember&co a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen)))))))
               (a-friend (lambda (x y)
                           (null? y))))
         (multirember&co 'a (__toplevel_cons 'a ()) a-friend)
         (multirember&co
            'a
            ()
            (lambda (newlat seen)
               (a-friend newlat (cons (car (__toplevel_cons 'a ())) seen))))
         ((lambda (newlat seen) (a-friend newlat (cons (car (__toplevel_cons 'a ())) seen))) () ())
         (multirember&co 'a (__toplevel_cons 'b (__toplevel_cons 'a ())) a-friend)
         (multirember&co
            'a
            (__toplevel_cons 'a ())
            (lambda (newlat seen)
               (a-friend (cons (car (__toplevel_cons 'b (__toplevel_cons 'a ()))) newlat) seen)))
         (multirember&co
            'a
            ()
            (lambda (newlat seen)
               ((lambda (newlat seen) (a-friend (cons (car (__toplevel_cons 'b (__toplevel_cons 'a ()))) newlat) seen))
                  newlat
                  (cons (car (__toplevel_cons 'a ())) seen))))
         ((lambda (newlat seen)
            ((lambda (newlat seen) (a-friend (cons (car (__toplevel_cons 'b (__toplevel_cons 'a ()))) newlat) seen))
               newlat
               (cons (car (__toplevel_cons 'a ())) seen)))
            ()
            ())
         (<change>
            (multirember&co
               'a
               (__toplevel_cons
                  'a
                  (__toplevel_cons
                     'b
                     (__toplevel_cons
                        'c
                        (__toplevel_cons 'd (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'a ())))))))
               (lambda (x y)
                  (length x)))
            ())
         (<change>
            ()
            cons)
         (<change>
            (multirember&co
               'a
               (__toplevel_cons
                  'a
                  (__toplevel_cons
                     'b
                     (__toplevel_cons
                        'c
                        (__toplevel_cons 'd (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'a ())))))))
               (lambda (x y)
                  (display (cons "x:" x))))
            ())
         (multirember&co
            'a
            (__toplevel_cons
               'a
               (__toplevel_cons
                  'b
                  (__toplevel_cons
                     'c
                     (__toplevel_cons 'd (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'a ())))))))
            (lambda (x y)
               (display (cons "y:" y))))
         (letrec ((multiinsertL (lambda (new old lat)
                                  (if (null? lat)
                                     ()
                                     (if (eq? old (car lat))
                                        (cons new (cons old (multiinsertL new old (cdr lat))))
                                        (cons (car lat) (multiinsertL new old (car lat)))))))
                  (multiinsertR (lambda (new old lat)
                                  (if (null? lat)
                                     ()
                                     (if (eq? old (car lat))
                                        (cons old (cons new (multiinsertR new old (cdr lat))))
                                        (cons (car lat) (multiinsertR new old (cdr lat)))))))
                  (multiinsertLR (lambda (new oldL oldR lat)
                                   (if (null? lat)
                                      ()
                                      (if (eq? (car lat) oldL)
                                         (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))
                                         (if (eq? (car lat) oldR)
                                            (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))
                                            (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))))))
                  (add1 (lambda (x)
                          (+ x 1)))
                  (multiinsertLR&co (lambda (new oldL oldR lat col)
                                      (if (null? lat)
                                         (col () 0 0)
                                         (if (eq? (car lat) oldL)
                                            (multiinsertLR&co
                                               new
                                               oldL
                                               oldR
                                               (cdr lat)
                                               (lambda (newlat L R)
                                                  (col (cons new (cons oldL newlat)) (add1 L) R)))
                                            (if (eq? (car lat) oldR)
                                               (multiinsertLR&co
                                                  new
                                                  oldL
                                                  oldR
                                                  (cdr lat)
                                                  (lambda (newlat L R)
                                                     (col (cons oldR (cons new newlat)) L (add1 R))))
                                               (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R) (col (cons (car lat) newlat) L R)))))))))
            (multiinsertLR&co
               'salty
               'fish
               'chips
               (__toplevel_cons
                  'chips
                  (__toplevel_cons
                     'and
                     (__toplevel_cons
                        'fish
                        (__toplevel_cons 'or (__toplevel_cons 'fish (__toplevel_cons 'and (__toplevel_cons 'chips ())))))))
               (lambda (newlat Lcount Rcount)
                  newlat))
            (letrec ((sub1 (lambda (n)
                             (- n 1)))
                     (o+ (lambda (n m)
                           (if (zero? m) n (add1 (o+ n (sub1 m))))))
                     (o- (lambda (n m)
                           (if (zero? m) n (sub1 (o- n (sub1 m))))))
                     (o* (lambda (n m)
                           (if (zero? m) 0 (o+ n (o* n (sub1 m))))))
                     (o/ (lambda (n m)
                           (if (< n m) 0 (add1 (o/ (o- n m) m)))))
                     (my-even? (lambda (n)
                                 (= (o* (o/ n 2) 2) n)))
                     (evens-only* (lambda (l)
                                    (if (null? l)
                                       ()
                                       (if (atom? (car l))
                                          (if (even? (car l))
                                             (cons (car l) (evens-only* (cdr l)))
                                             (evens-only* (cdr l)))
                                          (cons (evens-only* (car l)) (evens-only* (cdr l)))))))
                     (evens-only*&co (lambda (l col)
                                       (if (null? l)
                                          (col () 1 0)
                                          (if (atom? (car l))
                                             (if (even? (car l))
                                                (evens-only*&co (cdr l) (lambda (newl p s) (col (cons (car l) newl) (* (car l) p) s)))
                                                (evens-only*&co (cdr l) (lambda (newl p s) (col newl p (+ (car l) s)))))
                                             (evens-only*&co
                                                (car l)
                                                (lambda (al ap as)
                                                   (evens-only*&co (cdr l) (lambda (dl dp ds) (col (cons al dl) (* ap dp) (+ as ds)))))))))))
               (evens-only*&co
                  (__toplevel_cons
                     (__toplevel_cons 9 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 8 ()))))
                     (__toplevel_cons
                        3
                        (__toplevel_cons
                           10
                           (__toplevel_cons
                              (__toplevel_cons
                                 (__toplevel_cons 9 (__toplevel_cons 9 ()))
                                 (__toplevel_cons 7 (__toplevel_cons 6 ())))
                              (__toplevel_cons 2 ())))))
                  (lambda (newl product sum)
                     (cons sum (cons product newl)))))))))