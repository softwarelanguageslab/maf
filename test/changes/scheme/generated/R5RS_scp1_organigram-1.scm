; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 1
; * calls to id fun: 1
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
                                                             (if (<change> __or_res (not __or_res))
                                                                __or_res
                                                                (hierarchisch?-in path (cdr organigrammen)))))))
                                   (hierarchisch? (lambda (path organigram)
                                                    (<change>
                                                       (if (if (eq? p1 (baas organigram)) (member p2 path) #f)
                                                          #t
                                                          (if (if (eq? p2 (baas organigram)) (member p1 path) #f)
                                                             #t
                                                             (hierarchisch?-in (cons (baas organigram) path) (sub-organigrammen organigram))))
                                                       ((lambda (x) x)
                                                          (if (if (eq? p1 (baas organigram)) (member p2 path) #f)
                                                             #t
                                                             (if (if (eq? p2 (baas organigram)) (member p1 path) #f)
                                                                (<change>
                                                                   #t
                                                                   (hierarchisch?-in (cons (baas organigram) path) (sub-organigrammen organigram)))
                                                                (<change>
                                                                   (hierarchisch?-in (cons (baas organigram) path) (sub-organigrammen organigram))
                                                                   #t))))))))
                             (hierarchisch? () organigram))))
         (collegas (lambda (p organigram)
                     (letrec ((collegas-in (lambda (oversten organigrammen)
                                             (if (<change> (null? organigrammen) (not (null? organigrammen)))
                                                #f
                                                (let ((__or_res (collegas oversten (car organigrammen))))
                                                   (<change>
                                                      ()
                                                      organigrammen)
                                                   (if __or_res
                                                      __or_res
                                                      (collegas-in oversten (cdr organigrammen)))))))
                              (werknemers-in (lambda (organigrammen)
                                               (if (null? organigrammen)
                                                  ()
                                                  (append (werknemers (car organigrammen)) (werknemers-in (cdr organigrammen))))))
                              (werknemers (lambda (organigram)
                                            (cons (baas organigram) (werknemers-in (sub-organigrammen organigram)))))
                              (collegas (lambda (oversten organigram)
                                          (if (eq? p (baas organigram))
                                             (append oversten (werknemers-in (sub-organigrammen organigram)))
                                             (collegas-in (cons (baas organigram) oversten) (sub-organigrammen organigram))))))
                        (collegas () organigram)))))
   (if (hierarchisch? 'directeur 'verkoopsleider-brussel organigram)
      (if (hierarchisch? 'bediende1 'hoofd-productie organigram)
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