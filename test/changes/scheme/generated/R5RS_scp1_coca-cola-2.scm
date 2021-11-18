; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 3
; * swapped branches: 1
; * calls to id fun: 2
(letrec ((foldr (lambda (f base lst)
                  (letrec ((foldr-aux (lambda (lst)
                                        (if (null? lst)
                                           base
                                           (f (car lst) (foldr-aux (cdr lst)))))))
                     (foldr-aux lst))))
         (atom? (lambda (x)
                  (not (pair? x))))
         (Coca-Cola-NV (__toplevel_cons
                         'Coca-Cola-NV
                         (__toplevel_cons
                            (__toplevel_cons
                               'Frisdranken
                               (__toplevel_cons
                                  (__toplevel_cons
                                     'Coca-Cola
                                     (__toplevel_cons
                                        (__toplevel_cons
                                           'Regular-Coca-Cola
                                           (__toplevel_cons (__toplevel_cons 'Coke (__toplevel_cons (__toplevel_cons 10000000 ()) ())) ()))
                                        (__toplevel_cons
                                           (__toplevel_cons
                                              'light-Coca-Cola
                                              (__toplevel_cons
                                                 (__toplevel_cons 'Coke-Light (__toplevel_cons (__toplevel_cons 800000 ()) ()))
                                                 (__toplevel_cons (__toplevel_cons 'Coke-Zero (__toplevel_cons (__toplevel_cons 200000 ()) ())) ())))
                                           ())))
                                  (__toplevel_cons
                                     (__toplevel_cons
                                        'Fanta
                                        (__toplevel_cons
                                           (__toplevel_cons 'Fanta-Orange (__toplevel_cons (__toplevel_cons 800000 ()) ()))
                                           (__toplevel_cons
                                              (__toplevel_cons 'Fanta-Lemon (__toplevel_cons (__toplevel_cons 200000 ()) ()))
                                              ())))
                                     (__toplevel_cons
                                        (__toplevel_cons
                                           'Sprite
                                           (__toplevel_cons
                                              (__toplevel_cons 'Sprite-Zero (__toplevel_cons (__toplevel_cons 1000000 ()) ()))
                                              ()))
                                        ()))))
                            (__toplevel_cons
                               (__toplevel_cons
                                  'Sappen
                                  (__toplevel_cons
                                     (__toplevel_cons
                                        'Minute-Maid
                                        (__toplevel_cons
                                           (__toplevel_cons 'Minute-Maid-Sinaas (__toplevel_cons (__toplevel_cons 2000000 ()) ()))
                                           (__toplevel_cons
                                              (__toplevel_cons 'Minute-Maid-Tomaat (__toplevel_cons (__toplevel_cons 1000000 ()) ()))
                                              ())))
                                     ()))
                               ()))))
         (omzetcijfer (lambda (categorie)
                        (caadr categorie)))
         (heeft-omzetcijfer (lambda (categorie)
                              (if (pair? categorie)
                                 (if (pair? (cadr categorie))
                                    (if (atom? (caadr categorie))
                                       (number? (caadr categorie))
                                       #f)
                                    #f)
                                 #f)))
         (deel-categorien (lambda (categorie)
                            (cdr categorie)))
         (hoofdcategorie (lambda (categorie)
                           (<change>
                              ()
                              (display car))
                           (<change>
                              (car categorie)
                              ((lambda (x) x) (car categorie)))))
         (bereken (lambda (lst)
                    (<change>
                       ()
                       (display +))
                    (if (null? lst)
                       0
                       (if (atom? lst)
                          0
                          (if (number? (car lst))
                             (car lst)
                             (+ (bereken (car lst)) (bereken (cdr lst))))))))
         (omzet (lambda (bedrijf categorie)
                  (if (eq? (hoofdcategorie bedrijf) categorie)
                     (<change>
                        (bereken bedrijf)
                        (omzet-in (deel-categorien bedrijf) categorie))
                     (<change>
                        (omzet-in (deel-categorien bedrijf) categorie)
                        (bereken bedrijf)))))
         (omzet-in (lambda (lst categorie)
                     (if (null? lst)
                        #f
                        (let ((__or_res (omzet (car lst) categorie)))
                           (if __or_res
                              __or_res
                              (omzet-in (cdr lst) categorie))))))
         (collect-pairs (lambda (bedrijf)
                          (if (heeft-omzetcijfer bedrijf)
                             (list (list (hoofdcategorie bedrijf) (omzetcijfer bedrijf)))
                             (collect-pairs-in (deel-categorien bedrijf)))))
         (collect-pairs-in (lambda (lst)
                             (if (null? lst)
                                ()
                                (append (collect-pairs (car lst)) (collect-pairs-in (cdr lst))))))
         (verdeel-democratisch (lambda (bedrijf budget)
                                 (let* ((pairs (collect-pairs bedrijf))
                                        (total (foldr + 0 (map cadr pairs)))
                                        (factor (/ budget total)))
                                    (<change>
                                       ()
                                       (display (list (car x) (* factor (cadr x)))))
                                    (map (lambda (x) (list (car x) (* factor (cadr x)))) pairs))))
         (verdeel (lambda (bedrijf budget)
                    (<change>
                       (if (heeft-omzetcijfer bedrijf)
                          (list (hoofdcategorie bedrijf) budget)
                          (let* ((rest (deel-categorien bedrijf))
                                 (new-budget (/ budget (length rest))))
                             (cons (hoofdcategorie bedrijf) (verdeel-in rest new-budget))))
                       ((lambda (x) x)
                          (if (<change> (heeft-omzetcijfer bedrijf) (not (heeft-omzetcijfer bedrijf)))
                             (list (hoofdcategorie bedrijf) budget)
                             (let* ((rest (deel-categorien bedrijf))
                                    (new-budget (/ budget (length rest))))
                                (cons (hoofdcategorie bedrijf) (verdeel-in rest new-budget))))))))
         (verdeel-in (lambda (lst budget)
                       (if (null? lst)
                          ()
                          (cons (verdeel (car lst) budget) (verdeel-in (cdr lst) budget))))))
   (if (<change> (= (omzet Coca-Cola-NV 'Coca-Cola) 11000000) (not (= (omzet Coca-Cola-NV 'Coca-Cola) 11000000)))
      (if (<change> (= (omzet Coca-Cola-NV 'Sprite) 1000000) (not (= (omzet Coca-Cola-NV 'Sprite) 1000000)))
         (if (= (omzet Coca-Cola-NV 'Minute-Maid) 3000000)
            (if (equal? (verdeel-democratisch Coca-Cola-NV 128000000) (__toplevel_cons (__toplevel_cons 'Coke (__toplevel_cons 80000000 ())) (__toplevel_cons (__toplevel_cons 'Coke-Light (__toplevel_cons 6400000 ())) (__toplevel_cons (__toplevel_cons 'Coke-Zero (__toplevel_cons 1600000 ())) (__toplevel_cons (__toplevel_cons 'Fanta-Orange (__toplevel_cons 6400000 ())) (__toplevel_cons (__toplevel_cons 'Fanta-Lemon (__toplevel_cons 1600000 ())) (__toplevel_cons (__toplevel_cons 'Sprite-Zero (__toplevel_cons 8000000 ())) (__toplevel_cons (__toplevel_cons 'Minute-Maid-Sinaas (__toplevel_cons 16000000 ())) (__toplevel_cons (__toplevel_cons 'Minute-Maid-Tomaat (__toplevel_cons 8000000 ())) ())))))))))
               (equal?
                  (verdeel Coca-Cola-NV 1200000)
                  (__toplevel_cons
                     'Coca-Cola-NV
                     (__toplevel_cons
                        (__toplevel_cons
                           'Frisdranken
                           (__toplevel_cons
                              (__toplevel_cons
                                 'Coca-Cola
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'Regular-Coca-Cola
                                       (__toplevel_cons (__toplevel_cons 'Coke (__toplevel_cons 100000 ())) ()))
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'light-Coca-Cola
                                          (__toplevel_cons
                                             (__toplevel_cons 'Coke-Light (__toplevel_cons 50000 ()))
                                             (__toplevel_cons (__toplevel_cons 'Coke-Zero (__toplevel_cons 50000 ())) ())))
                                       ())))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'Fanta
                                    (__toplevel_cons
                                       (__toplevel_cons 'Fanta-Orange (__toplevel_cons 100000 ()))
                                       (__toplevel_cons (__toplevel_cons 'Fanta-Lemon (__toplevel_cons 100000 ())) ())))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'Sprite
                                       (__toplevel_cons (__toplevel_cons 'Sprite-Zero (__toplevel_cons 200000 ())) ()))
                                    ()))))
                        (__toplevel_cons
                           (__toplevel_cons
                              'Sappen
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'Minute-Maid
                                    (__toplevel_cons
                                       (__toplevel_cons 'Minute-Maid-Sinaas (__toplevel_cons 300000 ()))
                                       (__toplevel_cons (__toplevel_cons 'Minute-Maid-Tomaat (__toplevel_cons 300000 ())) ())))
                                 ()))
                           ()))))
               #f)
            #f)
         #f)
      #f))