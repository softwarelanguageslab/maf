; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((familieboom (__toplevel_cons
                        'jan
                        (__toplevel_cons
                           (__toplevel_cons
                              'piet
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'frans
                                    (__toplevel_cons (__toplevel_cons 'tom ()) (__toplevel_cons (__toplevel_cons 'roel ()) ())))
                                 (__toplevel_cons (__toplevel_cons 'mie ()) ())))
                           (__toplevel_cons
                              (__toplevel_cons
                                 'bram
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'inge
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'bert
                                             (__toplevel_cons (__toplevel_cons 'ina ()) (__toplevel_cons (__toplevel_cons 'ilse ()) ())))
                                          (__toplevel_cons (__toplevel_cons 'bart ()) ())))
                                    (__toplevel_cons (__toplevel_cons 'iris ()) ())))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'joost
                                    (__toplevel_cons (__toplevel_cons 'else (__toplevel_cons (__toplevel_cons 'ilse ()) ())) ()))
                                 ())))))
         (familiehoofd (lambda (fam)
                         (car fam)))
         (kinderen (lambda (fam)
                     (<change>
                        (cdr fam)
                        ((lambda (x) x) (cdr fam)))))
         (laatste-nakomeling? (lambda (fam)
                                (null? (kinderen fam))))
         (verdeel-democratisch (lambda (boom budget)
                                 (letrec ((verdeel (lambda (boom)
                                                     (if (laatste-nakomeling? boom)
                                                        1
                                                        (+ 1 (verdeel-in (kinderen boom))))))
                                          (verdeel-in (lambda (lst)
                                                        (if (null? lst)
                                                           0
                                                           (+ (verdeel (car lst)) (verdeel-in (cdr lst)))))))
                                    (/ budget (verdeel-in (kinderen boom))))))
         (budget (lambda (boom budget-list)
                   (letrec ((budget-hulp (lambda (boom budget-list)
                                           (+ (car budget-list) (budget-hulp-in (kinderen boom) (cdr budget-list)))))
                            (budget-hulp-in (lambda (bomen budget-list)
                                              (<change>
                                                 ()
                                                 bomen)
                                              (<change>
                                                 (if (let ((__or_res (null? bomen))) (if __or_res __or_res (null? budget-list)))
                                                    0
                                                    (+ (budget-hulp (car bomen) budget-list) (budget-hulp-in (cdr bomen) budget-list)))
                                                 ((lambda (x) x)
                                                    (if (let ((__or_res (null? bomen))) (if __or_res __or_res (null? budget-list)))
                                                       0
                                                       (+ (budget-hulp (car bomen) budget-list) (budget-hulp-in (cdr bomen) budget-list))))))))
                      (budget-hulp-in (kinderen boom) budget-list))))
         (verdeel (lambda (boom budget)
                    (if (laatste-nakomeling? boom)
                       (list (list (familiehoofd boom) budget))
                       (let* ((rest (kinderen boom))
                              (new-budget (/ budget (length rest))))
                          (verdeel-in rest new-budget)))))
         (verdeel-in (lambda (bomen budget)
                       (if (null? bomen)
                          ()
                          (append (verdeel (car bomen) budget) (verdeel-in (cdr bomen) budget))))))
   (if (= (verdeel-democratisch familieboom 1500) 100)
      (if (<change> (= (budget familieboom (__toplevel_cons 100 (__toplevel_cons 50 (__toplevel_cons 20 ())))) 650) (not (= (budget familieboom (__toplevel_cons 100 (__toplevel_cons 50 (__toplevel_cons 20 ())))) 650)))
         (equal?
            (verdeel familieboom 3000)
            (__toplevel_cons
               (__toplevel_cons 'tom (__toplevel_cons 250 ()))
               (__toplevel_cons
                  (__toplevel_cons 'roel (__toplevel_cons 250 ()))
                  (__toplevel_cons
                     (__toplevel_cons 'mie (__toplevel_cons 500 ()))
                     (__toplevel_cons
                        (__toplevel_cons 'ina (__toplevel_cons 125 ()))
                        (__toplevel_cons
                           (__toplevel_cons 'ilse (__toplevel_cons 125 ()))
                           (__toplevel_cons
                              (__toplevel_cons 'bart (__toplevel_cons 250 ()))
                              (__toplevel_cons
                                 (__toplevel_cons 'iris (__toplevel_cons 500 ()))
                                 (__toplevel_cons (__toplevel_cons 'ilse (__toplevel_cons 1000 ())) ())))))))))
         #f)
      #f))