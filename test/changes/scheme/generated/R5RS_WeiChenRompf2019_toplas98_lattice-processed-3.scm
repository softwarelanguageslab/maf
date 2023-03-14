; Changes:
; * removed: 1
; * added: 3
; * swaps: 1
; * negated predicates: 2
; * swapped branches: 0
; * calls to id fun: 5
(letrec ((lexico (lambda (base)
                   (letrec ((lex-fixed (lambda (fixed lhs rhs)
                                         (letrec ((check (lambda (lhs rhs)
                                                           (<change>
                                                              (if (null? lhs)
                                                                 fixed
                                                                 (let ((probe (base (car lhs) (car rhs))))
                                                                    (if (let ((__or_res (eq? probe 'equal))) (if __or_res __or_res (eq? probe fixed)))
                                                                       (check (cdr lhs) (cdr rhs))
                                                                       'uncomparable)))
                                                              ((lambda (x) x)
                                                                 (if (null? lhs)
                                                                    fixed
                                                                    (let ((probe (base (car lhs) (car rhs))))
                                                                       (if (<change> (let ((__or_res (eq? probe 'equal))) (if __or_res __or_res (eq? probe fixed))) (not (let ((__or_res (eq? probe 'equal))) (if __or_res __or_res (eq? probe fixed)))))
                                                                          (check (cdr lhs) (cdr rhs))
                                                                          'uncomparable))))))))
                                            (check lhs rhs))))
                            (lex-first (lambda (lhs rhs)
                                         (<change>
                                            (if (null? lhs)
                                               'equal
                                               (let ((probe (base (car lhs) (car rhs))))
                                                  (if (let ((__or_res (eq? probe 'less))) (if __or_res __or_res (eq? probe 'more)))
                                                     (lex-fixed probe (cdr lhs) (cdr rhs))
                                                     (if (eq? probe 'equal)
                                                        (lex-first (cdr lhs) (cdr rhs))
                                                        (if (eq? probe 'uncomparable) 'uncomparable #f)))))
                                            ((lambda (x) x)
                                               (if (null? lhs)
                                                  'equal
                                                  (let ((probe (base (car lhs) (car rhs))))
                                                     (<change>
                                                        (if (let ((__or_res (eq? probe 'less))) (if __or_res __or_res (eq? probe 'more)))
                                                           (lex-fixed probe (cdr lhs) (cdr rhs))
                                                           (if (eq? probe 'equal)
                                                              (lex-first (cdr lhs) (cdr rhs))
                                                              (if (eq? probe 'uncomparable) 'uncomparable #f)))
                                                        ((lambda (x) x)
                                                           (if (let ((__or_res (eq? probe 'less))) (if __or_res __or_res (eq? probe 'more)))
                                                              (lex-fixed probe (cdr lhs) (cdr rhs))
                                                              (if (eq? probe 'equal)
                                                                 (lex-first (cdr lhs) (cdr rhs))
                                                                 (if (eq? probe 'uncomparable) 'uncomparable #f))))))))))))
                      lex-first)))
         (make-lattice (lambda (elem-list cmp-func)
                         (cons elem-list cmp-func)))
         (lattice->elements (lambda (l)
                              (car l)))
         (lattice->cmp (lambda (l)
                         (cdr l)))
         (zulu-select (lambda (test lst)
                        (<change>
                           (letrec ((select-a (lambda (ac lst)
                                                (if (null? lst)
                                                   (lattice-reverse! ac)
                                                   (select-a (let ((head (car lst))) (if (test head) (cons head ac) ac)) (cdr lst))))))
                              (select-a () lst))
                           ((lambda (x) x)
                              (letrec ((select-a (lambda (ac lst)
                                                   (if (null? lst)
                                                      (lattice-reverse! ac)
                                                      (select-a (let ((head (car lst))) (if (test head) (cons head ac) ac)) (cdr lst))))))
                                 (select-a () lst))))))
         (lattice-reverse! (letrec ((rotate (lambda (fo fum)
                                             (let ((next (cdr fo)))
                                                (set-cdr! fo fum)
                                                (if (null? next) fo (rotate next fo))))))
                             (lambda (lst)
                                (if (null? lst) () (rotate lst ())))))
         (select-map (lambda (test func lst)
                       (letrec ((select-a (lambda (ac lst)
                                            (if (null? lst)
                                               (lattice-reverse! ac)
                                               (select-a (let ((head (car lst))) (if (test head) (cons (func head) ac) ac)) (cdr lst))))))
                          (select-a () lst))))
         (map-and (lambda (proc lst)
                    (if (null? lst)
                       #t
                       (letrec ((drudge (lambda (lst)
                                          (let ((rest (cdr lst)))
                                             (if (null? rest)
                                                (proc (car lst))
                                                (if (proc (car lst)) (drudge rest) #f))))))
                          (drudge lst)))))
         (maps-1 (lambda (source target pas new)
                   (let ((scmp (lattice->cmp source))
                         (tcmp (lattice->cmp target)))
                      (let ((less (select-map (lambda (p) (eq? 'less (scmp (car p) new))) (lambda (l) (cdr l)) pas))
                            (more (select-map (lambda (p) (eq? 'more (scmp (car p) new))) (lambda (l) (cdr l)) pas)))
                         (zulu-select
                            (lambda (t)
                               (if (map-and (lambda (t2) (memq (tcmp t2 t) (__toplevel_cons 'less (__toplevel_cons 'equal ())))) less)
                                  (map-and (lambda (t2) (memq (tcmp t2 t) (__toplevel_cons 'more (__toplevel_cons 'equal ())))) more)
                                  #f))
                            (lattice->elements target))))))
         (maps-rest (lambda (source target pas rest to-1 to-collect)
                      (if (null? rest)
                         (to-1 pas)
                         (let ((next (car rest))
                               (rest (cdr rest)))
                            (to-collect
                               (map
                                  (lambda (x)
                                     (maps-rest source target (cons (cons next x) pas) rest to-1 to-collect))
                                  (maps-1 source target pas next)))))))
         (maps (lambda (source target)
                 (make-lattice
                    (maps-rest
                       source
                       target
                       ()
                       (lattice->elements source)
                       (lambda (x)
                          (list (map (lambda (l) (cdr l)) x)))
                       (lambda (x)
                          (apply append x)))
                    (lexico (lattice->cmp target)))))
         (print-frequency 10000)
         (count-maps (lambda (source target)
                       (<change>
                          ()
                          (newline))
                       (let ((count 0))
                          (maps-rest
                             source
                             target
                             ()
                             (lattice->elements source)
                             (lambda (x)
                                (<change>
                                   (set! count (+ count 1))
                                   (if (not (= 0 (remainder count print-frequency)))
                                      (begin
                                         (display (display count))
                                         (display "...")
                                         (newline))
                                      (void)))
                                (<change>
                                   (if (= 0 (remainder count print-frequency))
                                      (begin
                                         (display count)
                                         (display "...")
                                         (newline))
                                      (void))
                                   (set! count (+ count 1)))
                                (<change>
                                   ()
                                   (display "..."))
                                1)
                             (lambda (x)
                                ((letrec ((loop (lambda (i l) (if (null? l) i (loop (+ i (car l)) (cdr l)))))) loop) 0 x)))))))
   (let* ((l2 (make-lattice
                (__toplevel_cons 'low (__toplevel_cons 'high ()))
                (lambda (lhs rhs)
                   (<change>
                      (if (eq? lhs 'low)
                         (if (eq? rhs 'low)
                            'equal
                            (if (eq? rhs 'high)
                               'less
                               (error 'make-lattice "base" rhs)))
                         (if (eq? lhs 'high)
                            (if (eq? rhs 'low)
                               'more
                               (if (eq? rhs 'high)
                                  'equal
                                  (error 'make-lattice "base" rhs)))
                            (error 'make-lattice "base" lhs)))
                      ((lambda (x) x)
                         (if (eq? lhs 'low)
                            (if (eq? rhs 'low)
                               'equal
                               (if (eq? rhs 'high)
                                  'less
                                  (error 'make-lattice "base" rhs)))
                            (if (eq? lhs 'high)
                               (if (eq? rhs 'low)
                                  'more
                                  (if (eq? rhs 'high)
                                     'equal
                                     (error 'make-lattice "base" rhs)))
                               (error 'make-lattice "base" lhs)))))))))
      (display (count-maps l2 l2))))