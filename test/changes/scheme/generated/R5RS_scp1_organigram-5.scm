; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((organigram (__toplevel_cons
                       'directeur
                       (__toplevel_cons
                          (__toplevel_cons
                             'hoofd-verkoop
                             (__toplevel_cons
                                (__toplevel_cons 'verkoopsleider-vlaanderen ())
                                (__toplevel_cons (__toplevel_cons 'verkoopsleider-brussel ()) ())))
                          (__toplevel_cons
                             (__toplevel_cons
                                'hoofd-productie
                                (__toplevel_cons
                                   (__toplevel_cons
                                      'hoofd-inkoop
                                      (__toplevel_cons
                                         (__toplevel_cons 'bediende1 ())
                                         (__toplevel_cons
                                            (__toplevel_cons 'bediende2 ())
                                            (__toplevel_cons (__toplevel_cons 'bediende3 ()) ()))))
                                   (__toplevel_cons (__toplevel_cons 'hoofd-fakturen ()) ())))
                             (__toplevel_cons
                                (__toplevel_cons
                                   'hoofd-administratie
                                   (__toplevel_cons
                                      (__toplevel_cons 'hoofd-personeel ())
                                      (__toplevel_cons (__toplevel_cons 'hoofd-boekhouding ()) ())))
                                ())))))
         (baas (lambda (organigram)
                 (car organigram)))
         (sub-organigrammen (lambda (organigram)
                              (cdr organigram)))
         (hierarchisch? (lambda (p1 p2 organigram)
                          (letrec ((hierarchisch?-in (lambda (path organigrammen)
                                                       (if (null? organigrammen)
                                                          #f
                                                          (let ((__or_res (hierarchisch? path (car organigrammen))))
                                                             (if __or_res
                                                                __or_res
                                                                (hierarchisch?-in path (cdr organigrammen)))))))
                                   (hierarchisch? (lambda (path organigram)
                                                    (if (if (eq? p1 (baas organigram)) (member p2 path) #f)
                                                       #t
                                                       (if (if (eq? p2 (baas organigram)) (member p1 path) #f)
                                                          #t
                                                          (hierarchisch?-in (cons (baas organigram) path) (sub-organigrammen organigram)))))))
                             (hierarchisch? () organigram))))
         (collegas (lambda (p organigram)
                     (letrec ((collegas-in (lambda (oversten organigrammen)
                                             (if (null? organigrammen)
                                                #f
                                                (let ((__or_res (collegas oversten (car organigrammen))))
                                                   (if __or_res
                                                      __or_res
                                                      (collegas-in oversten (cdr organigrammen)))))))
                              (werknemers-in (lambda (organigrammen)
                                               (<change>
                                                  ()
                                                  (null? organigrammen))
                                               (if (null? organigrammen)
                                                  ()
                                                  (append (werknemers (car organigrammen)) (werknemers-in (cdr organigrammen))))))
                              (werknemers (lambda (organigram)
                                            (<change>
                                               ()
                                               (display werknemers-in))
                                            (cons (baas organigram) (werknemers-in (sub-organigrammen organigram)))))
                              (collegas (lambda (oversten organigram)
                                          (if (eq? p (baas organigram))
                                             (append oversten (werknemers-in (sub-organigrammen organigram)))
                                             (collegas-in (cons (baas organigram) oversten) (sub-organigrammen organigram))))))
                        (collegas () organigram)))))
   (if (hierarchisch? 'directeur 'verkoopsleider-brussel organigram)
      (if (<change> (hierarchisch? 'bediende1 'hoofd-productie organigram) (not (hierarchisch? 'bediende1 'hoofd-productie organigram)))
         (if (not (hierarchisch? 'hoofd-personeel 'bediende3 organigram))
            (equal?
               (collegas 'hoofd-inkoop organigram)
               (__toplevel_cons
                  'hoofd-productie
                  (__toplevel_cons
                     'directeur
                     (__toplevel_cons 'bediende1 (__toplevel_cons 'bediende2 (__toplevel_cons 'bediende3 ()))))))
            #f)
         #f)
      #f))