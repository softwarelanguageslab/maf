; Changes:
; * removed: 0
; * added: 1
; * swaps: 2
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (linebreak (lambda ()
                      (set! result (cons 'linebreak result))))
         (print-abc (lambda (a b c)
                      (<change>
                         ()
                         (display (output " ")))
                      (output a)
                      (output " ")
                      (output b)
                      (output " ")
                      (<change>
                         (output c)
                         (linebreak))
                      (<change>
                         (linebreak)
                         (output c))))
         (foo (lambda (a b c)
                (<change>
                   (print-abc a b c)
                   ((lambda (x) x) (print-abc a b c)))
                (<change>
                   (let ((a 4)
                         (c 5)
                         (b c))
                      (print-abc a b c)
                      (let ((b 6)
                            (c a))
                         (print-abc a b c))
                      (let ((a b)
                            (c a))
                         (print-abc a b c)))
                   (print-abc a b c))
                (<change>
                   (print-abc a b c)
                   (let ((a 4)
                         (c 5)
                         (b c))
                      (print-abc a b c)
                      (let ((b 6)
                            (c a))
                         (print-abc a b c))
                      (let ((a b)
                            (c a))
                         (print-abc a b c)))))))
   (foo 1 2 3)
   (equal?
      result
      (__toplevel_cons
         'linebreak
         (__toplevel_cons
            3
            (__toplevel_cons
               " "
               (__toplevel_cons
                  2
                  (__toplevel_cons
                     " "
                     (__toplevel_cons
                        1
                        (__toplevel_cons
                           'linebreak
                           (__toplevel_cons
                              4
                              (__toplevel_cons
                                 " "
                                 (__toplevel_cons
                                    3
                                    (__toplevel_cons
                                       " "
                                       (__toplevel_cons
                                          3
                                          (__toplevel_cons
                                             'linebreak
                                             (__toplevel_cons
                                                4
                                                (__toplevel_cons
                                                   " "
                                                   (__toplevel_cons
                                                      6
                                                      (__toplevel_cons
                                                         " "
                                                         (__toplevel_cons
                                                            4
                                                            (__toplevel_cons
                                                               'linebreak
                                                               (__toplevel_cons
                                                                  5
                                                                  (__toplevel_cons
                                                                     " "
                                                                     (__toplevel_cons
                                                                        3
                                                                        (__toplevel_cons
                                                                           " "
                                                                           (__toplevel_cons
                                                                              4
                                                                              (__toplevel_cons
                                                                                 'linebreak
                                                                                 (__toplevel_cons
                                                                                    3
                                                                                    (__toplevel_cons " " (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 1 ()))))))))))))))))))))))))))))))))