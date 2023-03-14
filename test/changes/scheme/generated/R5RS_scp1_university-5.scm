; Changes:
; * removed: 1
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((result ())
         (display2 (lambda (i)
                     (set! result (cons i result))))
         (newline2 (lambda ()
                     (set! result (cons 'newline result))))
         (VUBOrganigram (__toplevel_cons
                          'VUB
                          (__toplevel_cons
                             (__toplevel_cons
                                'academisch
                                (__toplevel_cons
                                   (__toplevel_cons 'rectoraat ())
                                   (__toplevel_cons
                                      (__toplevel_cons
                                         'faculteiten
                                         (__toplevel_cons
                                            (__toplevel_cons
                                               'rechten
                                               (__toplevel_cons
                                                  (__toplevel_cons
                                                     'bachelor
                                                     (__toplevel_cons
                                                        (__toplevel_cons 'ba-rechten ())
                                                        (__toplevel_cons (__toplevel_cons 'ba-criminologie ()) ())))
                                                  (__toplevel_cons
                                                     (__toplevel_cons
                                                        'master
                                                        (__toplevel_cons
                                                           (__toplevel_cons 'ma-rechten ())
                                                           (__toplevel_cons (__toplevel_cons 'ma-criminologie ()) ())))
                                                     ())))
                                            (__toplevel_cons
                                               (__toplevel_cons 'economie ())
                                               (__toplevel_cons
                                                  (__toplevel_cons
                                                     'wetenschappen
                                                     (__toplevel_cons
                                                        (__toplevel_cons
                                                           'bachelor
                                                           (__toplevel_cons
                                                              (__toplevel_cons 'ba-wiskunde ())
                                                              (__toplevel_cons (__toplevel_cons 'ba-fysica ()) (__toplevel_cons (__toplevel_cons 'ba-cw ()) ()))))
                                                        (__toplevel_cons
                                                           (__toplevel_cons
                                                              'master
                                                              (__toplevel_cons
                                                                 (__toplevel_cons 'ma-wiskunde ())
                                                                 (__toplevel_cons (__toplevel_cons 'ma-fysica ()) (__toplevel_cons (__toplevel_cons 'ma-cw ()) ()))))
                                                           ())))
                                                  ()))))
                                      ())))
                             (__toplevel_cons
                                (__toplevel_cons
                                   'administratief
                                   (__toplevel_cons
                                      (__toplevel_cons 'personeel ())
                                      (__toplevel_cons (__toplevel_cons 'financien ()) ())))
                                ()))))
         (display-n (lambda (n d)
                      (if (> n 0)
                         (begin
                            (display2 d)
                            (display-n (- n 1) d))
                         #f)))
         (print-lijn (lambda (aantalblanco tekst)
                       (<change>
                          (display-n aantalblanco " ")
                          ())
                       (display2 tekst)
                       (newline2)))
         (label (lambda (organigram)
                  (<change>
                     (car organigram)
                     ((lambda (x) x) (car organigram)))))
         (takken (lambda (organigram)
                   (cdr organigram)))
         (organigram-member-in (lambda (een-label organigrammen)
                                 (if (null? organigrammen)
                                    #f
                                    (let ((__or_res (organigram-member een-label (car organigrammen))))
                                       (<change>
                                          (if __or_res
                                             __or_res
                                             (organigram-member-in een-label (cdr organigrammen)))
                                          ((lambda (x) x) (if __or_res __or_res (organigram-member-in een-label (cdr organigrammen)))))))))
         (organigram-member (lambda (een-label organigram)
                              (<change>
                                 ()
                                 eq?)
                              (if (eq? een-label (label organigram))
                                 organigram
                                 (organigram-member-in een-label (takken organigram)))))
         (print (lambda (organigram)
                  (letrec ((print (lambda (diepte organigram)
                                    (print-lijn diepte (label organigram))
                                    (for-each (lambda (organigram) (print (+ diepte 1) organigram)) (takken organigram)))))
                     (<change>
                        ()
                        print)
                     (print 0 organigram))))
         (print-vanaf (lambda (organigram label)
                        (let ((res (organigram-member label organigram)))
                           (if res (print res) #f)))))
   (print-vanaf VUBOrganigram 'rechten)
   (letrec ((print-tot (lambda (organigram niveau)
                         (letrec ((print-tot (lambda (organigram niveau max-niveau)
                                               (if (<= niveau max-niveau)
                                                  (begin
                                                     (print-lijn niveau (label organigram))
                                                     (for-each (lambda (organigram) (print-tot organigram (+ niveau 1) max-niveau)) (takken organigram)))
                                                  #f))))
                            (print-tot organigram 0 niveau)))))
      (print-tot VUBOrganigram 2)
      (equal?
         result
         (__toplevel_cons
            'newline
            (__toplevel_cons
               'financien
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     " "
                     (__toplevel_cons
                        'newline
                        (__toplevel_cons
                           'personeel
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 " "
                                 (__toplevel_cons
                                    'newline
                                    (__toplevel_cons
                                       'administratief
                                       (__toplevel_cons
                                          " "
                                          (__toplevel_cons
                                             'newline
                                             (__toplevel_cons
                                                'faculteiten
                                                (__toplevel_cons
                                                   " "
                                                   (__toplevel_cons
                                                      " "
                                                      (__toplevel_cons
                                                         'newline
                                                         (__toplevel_cons
                                                            'rectoraat
                                                            (__toplevel_cons
                                                               " "
                                                               (__toplevel_cons
                                                                  " "
                                                                  (__toplevel_cons
                                                                     'newline
                                                                     (__toplevel_cons
                                                                        'academisch
                                                                        (__toplevel_cons
                                                                           " "
                                                                           (__toplevel_cons
                                                                              'newline
                                                                              (__toplevel_cons
                                                                                 'VUB
                                                                                 (__toplevel_cons
                                                                                    'newline
                                                                                    (__toplevel_cons
                                                                                       'ma-criminologie
                                                                                       (__toplevel_cons
                                                                                          " "
                                                                                          (__toplevel_cons
                                                                                             " "
                                                                                             (__toplevel_cons
                                                                                                'newline
                                                                                                (__toplevel_cons
                                                                                                   'ma-rechten
                                                                                                   (__toplevel_cons
                                                                                                      " "
                                                                                                      (__toplevel_cons
                                                                                                         " "
                                                                                                         (__toplevel_cons
                                                                                                            'newline
                                                                                                            (__toplevel_cons
                                                                                                               'master
                                                                                                               (__toplevel_cons
                                                                                                                  " "
                                                                                                                  (__toplevel_cons
                                                                                                                     'newline
                                                                                                                     (__toplevel_cons
                                                                                                                        'ba-criminologie
                                                                                                                        (__toplevel_cons
                                                                                                                           " "
                                                                                                                           (__toplevel_cons
                                                                                                                              " "
                                                                                                                              (__toplevel_cons
                                                                                                                                 'newline
                                                                                                                                 (__toplevel_cons
                                                                                                                                    'ba-rechten
                                                                                                                                    (__toplevel_cons
                                                                                                                                       " "
                                                                                                                                       (__toplevel_cons
                                                                                                                                          " "
                                                                                                                                          (__toplevel_cons
                                                                                                                                             'newline
                                                                                                                                             (__toplevel_cons
                                                                                                                                                'bachelor
                                                                                                                                                (__toplevel_cons " " (__toplevel_cons 'newline (__toplevel_cons 'rechten ())))))))))))))))))))))))))))))))))))))))))))))))))))