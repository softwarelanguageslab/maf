; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 3
(letrec ((lexico (lambda (base)
                   (letrec ((lex-fixed (lambda (fixed lhs1 rhs1)
                                         (letrec ((check (lambda (lhs2 rhs2)
                                                           (if (null? lhs2)
                                                              fixed
                                                              (let ((probe (base (car lhs2) (car rhs2))))
                                                                 (if (let ((__or_res (eq? probe 'equal))) (if __or_res __or_res (eq? probe fixed)))
                                                                    (check (cdr lhs2) (cdr rhs2))
                                                                    'uncomparable))))))
                                            (check lhs1 rhs1))))
                            (lex-first (lambda (lhs3 rhs3)
                                         (if (null? lhs3)
                                            'equal
                                            (let ((probe (base (car lhs3) (car rhs3))))
                                               (if (let ((__or_res (eq? probe 'less))) (if __or_res __or_res (eq? probe 'more)))
                                                  (lex-fixed probe (cdr lhs3) (cdr rhs3))
                                                  (if (eq? probe 'equal)
                                                     (lex-first (cdr lhs3) (cdr rhs3))
                                                     (if (eq? probe 'uncomparable) 'uncomparable #f))))))))
                      lex-first)))
         (make-lattice (lambda (elem-list cmp-func)
                         (cons elem-list cmp-func)))
         (lattice->elements (lambda (l1)
                              (car l1)))
         (lattice->cmp (lambda (l2)
                         (cdr l2)))
         (lattice-reverse! (letrec ((rotate (lambda (fo fum)
                                             (let ((next1 (cdr fo)))
                                                (set-cdr! fo fum)
                                                (<change>
                                                   (if (null? next1) fo (rotate next1 fo))
                                                   ((lambda (x) x) (if (null? next1) fo (rotate next1 fo))))))))
                             (lambda (lst1)
                                (<change>
                                   ()
                                   null?)
                                (if (null? lst1) () (rotate lst1 ())))))
         (zulu-select (lambda (test1 lst2)
                        (letrec ((select-a-1 (lambda (ac1 lst3)
                                               (if (null? lst3)
                                                  (lattice-reverse! ac1)
                                                  (select-a-1 (let ((head1 (car lst3))) (if (test1 head1) (cons head1 ac1) ac1)) (cdr lst3))))))
                           (select-a-1 () lst2))))
         (select-map (lambda (test2 func lst4)
                       (letrec ((select-a-2 (lambda (ac2 lst5)
                                              (if (null? lst5)
                                                 (lattice-reverse! ac2)
                                                 (select-a-2 (let ((head2 (car lst5))) (if (test2 head2) (cons (func head2) ac2) ac2)) (cdr lst5))))))
                          (select-a-2 () lst4))))
         (map-and (lambda (proc lst6)
                    (if (null? lst6)
                       #t
                       (letrec ((drudge (lambda (lst7)
                                          (let ((rest (cdr lst7)))
                                             (if (null? rest)
                                                (proc (car lst7))
                                                (if (proc (car lst7)) (drudge rest) #f))))))
                          (drudge lst6)))))
         (maps-1 (lambda (source target pas new)
                   (let ((scmp (lattice->cmp source))
                         (tcmp (lattice->cmp target)))
                      (let ((less (select-map (lambda (p1) (eq? 'less (scmp (car p1) new))) (lambda (l4) (cdr l4)) pas))
                            (more (select-map (lambda (p2) (eq? 'more (scmp (car p2) new))) (lambda (l5) (cdr l5)) pas)))
                         (zulu-select
                            (lambda (t)
                               (if (map-and (lambda (t2-1) (memq (tcmp t2-1 t) (__toplevel_cons 'less (__toplevel_cons 'equal ())))) less)
                                  (map-and
                                     (lambda (t2-2)
                                        (memq (tcmp t2-2 t) (__toplevel_cons 'more (__toplevel_cons 'equal ()))))
                                     more)
                                  #f))
                            (lattice->elements target))))))
         (maps-rest (lambda (source target pas rest to-1 to-collect)
                      (<change>
                         (if (null? rest)
                            (to-1 pas)
                            (let ((next2 (car rest))
                                  (rest (cdr rest)))
                               (to-collect
                                  (map
                                     (lambda (x1)
                                        (maps-rest source target (cons (cons next2 x1) pas) rest to-1 to-collect))
                                     (maps-1 source target pas next2)))))
                         ((lambda (x) x)
                            (if (null? rest)
                               (to-1 pas)
                               (let ((next2 (car rest))
                                     (rest (cdr rest)))
                                  (to-collect
                                     (map
                                        (lambda (x1)
                                           (<change>
                                              (maps-rest source target (cons (cons next2 x1) pas) rest to-1 to-collect)
                                              ((lambda (x) x) (maps-rest source target (cons (cons next2 x1) pas) rest to-1 to-collect))))
                                        (maps-1 source target pas next2)))))))))
         (maps (lambda (source target)
                 (make-lattice
                    (maps-rest
                       source
                       target
                       ()
                       (lattice->elements source)
                       (lambda (x2)
                          (list (map (lambda (l6) (cdr l)) x2)))
                       (lambda (x3)
                          (<change>
                             ()
                             apply)
                          (apply append x3)))
                    (lexico (lattice->cmp target)))))
         (print-frequency 10000)
         (count-maps (lambda (source target)
                       (let ((count 0))
                          (maps-rest
                             source
                             target
                             ()
                             (lattice->elements source)
                             (lambda (x4)
                                (set! count (+ count 1))
                                (if (= 0 (remainder count print-frequency))
                                   (begin
                                      (display count)
                                      (display "...")
                                      (newline))
                                   (void))
                                1)
                             (lambda (x5)
                                ((letrec ((loop (lambda (i l) (if (null? l) i (loop (+ i (car l)) (cdr l)))))) loop) 0 x5)))))))
   (let* ((l3 (make-lattice
                (__toplevel_cons 'low (__toplevel_cons 'high ()))
                (lambda (lhs4 rhs4)
                   (if (eq? lhs4 'low)
                      (if (eq? rhs4 'low)
                         'equal
                         (if (eq? rhs4 'high)
                            'less
                            (error "make-lattice")))
                      (if (eq? lhs4 'high)
                         (if (eq? rhs4 'low)
                            'more
                            (if (eq? rhs4 'high)
                               'equal
                               (error "make-lattice")))
                         (error "make-lattice")))))))
      (display (count-maps l3 l3))))