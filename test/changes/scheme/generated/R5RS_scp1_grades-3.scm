; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 2
; * calls to id fun: 0
(letrec ((show (lambda (namen punten test?)
                 (if (null? namen)
                    ()
                    (let ((res (show (cdr namen) (cdr punten) test?)))
                       (if (test? (car punten))
                          (cons (car namen) res)
                          res)))))
         (one (lambda (namen punten)
                (letrec ((één-buis? (lambda (punten)
                                      (if (null? punten)
                                         #f
                                         (let ((punt (car punten))
                                               (rest (cdr punten)))
                                            (if (< punt 10)
                                               (geen-buis? rest)
                                               (één-buis? rest))))))
                         (geen-buis? (lambda (punten)
                                       (if (null? punten)
                                          (<change>
                                             #t
                                             (let ((punt (car punten))
                                                   (rest (cdr punten)))
                                                (if (< punt 10) (geen-buis? rest) #f)))
                                          (<change>
                                             (let ((punt (car punten))
                                                   (rest (cdr punten)))
                                                (if (< punt 10) #f (geen-buis? rest)))
                                             #t)))))
                   (show namen punten één-buis?)))))
   (<change>
      ()
      (display 8))
   (<change>
      ()
      (display (__toplevel_cons 12 (__toplevel_cons 11 (__toplevel_cons 10 ())))))
   (<change>
      ()
      __toplevel_cons)
   (equal?
      (one
         (__toplevel_cons
            'wendy
            (__toplevel_cons 'dirk (__toplevel_cons 'kris (__toplevel_cons 'jan (__toplevel_cons 'eef ())))))
         (__toplevel_cons
            (__toplevel_cons 12 (__toplevel_cons 13 (__toplevel_cons 15 (__toplevel_cons 18 ()))))
            (__toplevel_cons
               (__toplevel_cons 7 (__toplevel_cons 10 (__toplevel_cons 14 (__toplevel_cons 17 ()))))
               (__toplevel_cons
                  (__toplevel_cons 13 (__toplevel_cons 8 (__toplevel_cons 7 (__toplevel_cons 11 ()))))
                  (__toplevel_cons
                     (__toplevel_cons 9 (__toplevel_cons 12 (__toplevel_cons 11 (__toplevel_cons 10 ()))))
                     (__toplevel_cons
                        (__toplevel_cons 18 (__toplevel_cons 14 (__toplevel_cons 17 (__toplevel_cons 19 ()))))
                        ()))))))
      (__toplevel_cons 'dirk (__toplevel_cons 'jan ()))))