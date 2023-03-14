; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((atom? (lambda (x)
                  (not (pair? x))))
         (mijn-vuurwerk (__toplevel_cons
                          'groen
                          (__toplevel_cons
                             (__toplevel_cons
                                (__toplevel_cons
                                   'blauw
                                   (__toplevel_cons
                                      (__toplevel_cons
                                         'X
                                         (__toplevel_cons
                                            (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ()))
                                            (__toplevel_cons 'X (__toplevel_cons 'X ()))))
                                      ()))
                                (__toplevel_cons
                                   (__toplevel_cons
                                      'rood
                                      (__toplevel_cons
                                         (__toplevel_cons
                                            (__toplevel_cons 'groen (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ()))
                                            (__toplevel_cons 'X ()))
                                         ()))
                                   (__toplevel_cons
                                      'X
                                      (__toplevel_cons
                                         (__toplevel_cons 'geel (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ()))
                                         ()))))
                             ())))
         (kleur (lambda (vuurwerk)
                  (car vuurwerk)))
         (takken (lambda (vuurwerk)
                   (cadr vuurwerk)))
         (low-energy? (lambda (vuurwerk)
                        (eq? vuurwerk 'X)))
         (tel-knallen (lambda (vuurwerk)
                        (if (null? vuurwerk)
                           0
                           (if (low-energy? vuurwerk)
                              0
                              (if (atom? vuurwerk)
                                 1
                                 (+ (tel-knallen (car vuurwerk)) (tel-knallen (cdr vuurwerk))))))))
         (tel-low-energies (lambda (v)
                             (if (<change> (null? v) (not (null? v)))
                                0
                                (if (low-energy? v)
                                   1
                                   (if (atom? v)
                                      0
                                      (+ (tel-low-energies (car v)) (tel-low-energies (cdr v))))))))
         (tel-einde-in (lambda (takken een-kleur)
                         (if (null? takken)
                            0
                            (if (low-energy? (car takken))
                               0
                               (+ (tel-einde (car takken) een-kleur) (tel-einde-in (cdr takken) een-kleur))))))
         (tel-einde (lambda (vuurwerk een-kleur)
                      (if (eq? (kleur vuurwerk) een-kleur)
                         (tel-low-energies (takken vuurwerk))
                         (tel-einde-in (takken vuurwerk) een-kleur))))
         (ster? (lambda (vuurwerk)
                  (not (member 'X (takken vuurwerk))))))
   (<change>
      (if (eq? (kleur mijn-vuurwerk) 'groen)
         (if (equal? (takken mijn-vuurwerk) (__toplevel_cons (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X (__toplevel_cons 'X ())))) ())) (__toplevel_cons (__toplevel_cons 'rood (__toplevel_cons (__toplevel_cons (__toplevel_cons 'groen (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X (__toplevel_cons (__toplevel_cons 'geel (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) ())))))
            (if (not (low-energy? mijn-vuurwerk))
               (if (low-energy? 'X)
                  (if (= (tel-knallen mijn-vuurwerk) 6)
                     (if (= (tel-einde mijn-vuurwerk 'blauw) 5)
                        (not (ster? mijn-vuurwerk))
                        #f)
                     #f)
                  #f)
               #f)
            #f)
         #f)
      ((lambda (x) x)
         (if (eq? (kleur mijn-vuurwerk) 'groen)
            (if (equal? (takken mijn-vuurwerk) (__toplevel_cons (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X (__toplevel_cons 'X ())))) ())) (__toplevel_cons (__toplevel_cons 'rood (__toplevel_cons (__toplevel_cons (__toplevel_cons 'groen (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X (__toplevel_cons (__toplevel_cons 'geel (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) ())))))
               (if (not (low-energy? mijn-vuurwerk))
                  (if (low-energy? 'X)
                     (if (= (tel-knallen mijn-vuurwerk) 6)
                        (if (= (tel-einde mijn-vuurwerk 'blauw) 5)
                           (not (ster? mijn-vuurwerk))
                           #f)
                        #f)
                     #f)
                  #f)
               #f)
            #f))))