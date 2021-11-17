; Changes:
; * removed: 4
; * added: 0
; * swaps: 5
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 3
(letrec ((result ())
         (display2 (lambda (item)
                     (set! result (cons item result))))
         (newline2 (lambda ()
                     (set! result (cons 'newline result))))
         (make-row (lambda (key name age wage)
                     (vector key name age wage)))
         (key-ref (lambda (row)
                    (vector-ref row 0)))
         (name-ref (lambda (row)
                     (vector-ref row 1)))
         (age-ref (lambda (row)
                    (vector-ref row 2)))
         (wage-ref (lambda (row)
                     (vector-ref row 3)))
         (key-set! (lambda (row value)
                     (vector-set! row 0 value)))
         (name-set! (lambda (row value)
                      (vector-set! row 1 value)))
         (age-set! (lambda (row value)
                     (vector-set! row 2 value)))
         (wage-set! (lambda (row value)
                      (vector-set! row 3 value)))
         (show-row (lambda (row)
                     (display2 "[Sleutel:")
                     (<change>
                        (display2 (key-ref row))
                        (display2 "]"))
                     (<change>
                        (display2 "]")
                        (display2 (key-ref row)))
                     (display2 "[Naam:")
                     (display2 (name-ref row))
                     (<change>
                        (display2 "]")
                        (display2 "[Leeftijd:"))
                     (<change>
                        (display2 "[Leeftijd:")
                        (display2 "]"))
                     (display2 (age-ref row))
                     (<change>
                        (display2 "]")
                        (display2 "[Salaris:"))
                     (<change>
                        (display2 "[Salaris:")
                        (display2 "]"))
                     (<change>
                        (display2 (wage-ref row))
                        (display2 "]"))
                     (<change>
                        (display2 "]")
                        (display2 (wage-ref row)))))
         (make-table (lambda (rows)
                       (<change>
                          (make-vector rows 0)
                          ((lambda (x) x) (make-vector rows 0)))))
         (table-size (lambda (table)
                       (vector-length table)))
         (row-ref (lambda (table pos)
                    (if (< pos (table-size table))
                       (vector-ref table pos)
                       #f)))
         (row-set! (lambda (table pos row)
                     (if (< pos (table-size table))
                        (vector-set! table pos row)
                        #f)))
         (show-table (lambda (table)
                       (letrec ((iter (lambda (index)
                                        (<change>
                                           (if (= index (table-size table))
                                              (newline2)
                                              (begin
                                                 (show-row (row-ref table index))
                                                 (newline2)
                                                 (iter (+ index 1))))
                                           ((lambda (x) x)
                                              (if (= index (table-size table))
                                                 (newline2)
                                                 (begin
                                                    (<change>
                                                       (show-row (row-ref table index))
                                                       ())
                                                    (newline2)
                                                    (<change>
                                                       (iter (+ index 1))
                                                       ((lambda (x) x) (iter (+ index 1)))))))))))
                          (iter 0))))
         (table (make-table 10)))
   (row-set! table 0 (make-row 8 'Bernard 45 120000))
   (row-set! table 1 (make-row 3 'Dirk 26 93000))
   (row-set! table 2 (make-row 6 'George 48 130000))
   (<change>
      (row-set! table 3 (make-row 6 'Greet 27 75000))
      (row-set! table 4 (make-row 1 'Kaat 18 69000)))
   (<change>
      (row-set! table 4 (make-row 1 'Kaat 18 69000))
      (row-set! table 3 (make-row 6 'Greet 27 75000)))
   (row-set! table 5 (make-row 5 'Mauranne 21 69000))
   (row-set! table 6 (make-row 4 'Peter 33 80000))
   (row-set! table 7 (make-row 2 'Piet 25 96000))
   (<change>
      (row-set! table 8 (make-row 9 'Tom 26 96000))
      ())
   (<change>
      (row-set! table 9 (make-row 6 'Veronique 36 115000))
      ())
   (letrec ((expected-result (__toplevel_cons
                               'newline
                               (__toplevel_cons
                                  'newline
                                  (__toplevel_cons
                                     "]"
                                     (__toplevel_cons
                                        115000
                                        (__toplevel_cons
                                           "[Salaris:"
                                           (__toplevel_cons
                                              "]"
                                              (__toplevel_cons
                                                 36
                                                 (__toplevel_cons
                                                    "[Leeftijd:"
                                                    (__toplevel_cons
                                                       "]"
                                                       (__toplevel_cons
                                                          'Veronique
                                                          (__toplevel_cons
                                                             "[Naam:"
                                                             (__toplevel_cons
                                                                "]"
                                                                (__toplevel_cons
                                                                   6
                                                                   (__toplevel_cons
                                                                      "[Sleutel:"
                                                                      (__toplevel_cons
                                                                         'newline
                                                                         (__toplevel_cons
                                                                            "]"
                                                                            (__toplevel_cons
                                                                               96000
                                                                               (__toplevel_cons
                                                                                  "[Salaris:"
                                                                                  (__toplevel_cons
                                                                                     "]"
                                                                                     (__toplevel_cons
                                                                                        26
                                                                                        (__toplevel_cons
                                                                                           "[Leeftijd:"
                                                                                           (__toplevel_cons
                                                                                              "]"
                                                                                              (__toplevel_cons
                                                                                                 'Tom
                                                                                                 (__toplevel_cons
                                                                                                    "[Naam:"
                                                                                                    (__toplevel_cons
                                                                                                       "]"
                                                                                                       (__toplevel_cons
                                                                                                          9
                                                                                                          (__toplevel_cons
                                                                                                             "[Sleutel:"
                                                                                                             (__toplevel_cons
                                                                                                                'newline
                                                                                                                (__toplevel_cons
                                                                                                                   "]"
                                                                                                                   (__toplevel_cons
                                                                                                                      96000
                                                                                                                      (__toplevel_cons
                                                                                                                         "[Salaris:"
                                                                                                                         (__toplevel_cons
                                                                                                                            "]"
                                                                                                                            (__toplevel_cons
                                                                                                                               25
                                                                                                                               (__toplevel_cons
                                                                                                                                  "[Leeftijd:"
                                                                                                                                  (__toplevel_cons
                                                                                                                                     "]"
                                                                                                                                     (__toplevel_cons
                                                                                                                                        'Piet
                                                                                                                                        (__toplevel_cons
                                                                                                                                           "[Naam:"
                                                                                                                                           (__toplevel_cons
                                                                                                                                              "]"
                                                                                                                                              (__toplevel_cons
                                                                                                                                                 2
                                                                                                                                                 (__toplevel_cons
                                                                                                                                                    "[Sleutel:"
                                                                                                                                                    (__toplevel_cons
                                                                                                                                                       'newline
                                                                                                                                                       (__toplevel_cons
                                                                                                                                                          "]"
                                                                                                                                                          (__toplevel_cons
                                                                                                                                                             80000
                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                "[Salaris:"
                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                   "]"
                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                      33
                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                         "[Leeftijd:"
                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                            "]"
                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                               'Peter
                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                  "[Naam:"
                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                     "]"
                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                        4
                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                           "[Sleutel:"
                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                              'newline
                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                 "]"
                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                    69000
                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                       "[Salaris:"
                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                          "]"
                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                             21
                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                "[Leeftijd:"
                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                   "]"
                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                      'Mauranne
                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                         "[Naam:"
                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                            "]"
                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                               5
                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                  "[Sleutel:"
                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                     'newline
                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                        "]"
                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                           69000
                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                              "[Salaris:"
                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                 "]"
                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                    18
                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                       "[Leeftijd:"
                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                          "]"
                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                             'Kaat
                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                "[Naam:"
                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                   "]"
                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                      1
                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                         "[Sleutel:"
                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                            'newline
                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                               "]"
                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                  75000
                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                     "[Salaris:"
                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                        "]"
                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                           27
                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                              "[Leeftijd:"
                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                 "]"
                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                    'Greet
                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                       "[Naam:"
                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                          "]"
                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                             6
                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                "[Sleutel:"
                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                   'newline
                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                      "]"
                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                         130000
                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                            "[Salaris:"
                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                               "]"
                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                  48
                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                     "[Leeftijd:"
                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                        "]"
                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                           'George
                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                              "[Naam:"
                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                 "]"
                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                    6
                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                       "[Sleutel:"
                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                          'newline
                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                             "]"
                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                93000
                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                   "[Salaris:"
                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                      "]"
                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                         26
                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                            "[Leeftijd:"
                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                               "]"
                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                  'Dirk
                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                     "[Naam:"
                                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                        "]"
                                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                           3
                                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                              "[Sleutel:"
                                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                 'newline
                                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                    "]"
                                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                       120000
                                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                          "[Salaris:"
                                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                             "]"
                                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                45
                                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                   "[Leeftijd:"
                                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                      "]"
                                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                         'Bernard
                                                                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                            "[Naam:"
                                                                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons "]" (__toplevel_cons 8 (__toplevel_cons "[Sleutel:" ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      (<change>
         (show-table table)
         ())
      (equal? expected-result result)))