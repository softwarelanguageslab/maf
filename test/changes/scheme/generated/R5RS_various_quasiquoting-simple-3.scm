; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 2
; * calls to id fun: 0
(letrec ((day-of-week 'Sunday)
         (make-greeting (lambda ()
                          (__toplevel_cons
                             'Welcome
                             (__toplevel_cons
                                'to
                                (__toplevel_cons
                                   'the
                                   (__toplevel_cons
                                      'FooBar
                                      (__toplevel_cons
                                         'system!
                                         (__toplevel_cons
                                            'We
                                            (__toplevel_cons
                                               'hope
                                               (__toplevel_cons
                                                  'you
                                                  (__toplevel_cons
                                                     'enjoy
                                                     (__toplevel_cons
                                                        'your
                                                        (__toplevel_cons
                                                           'visit
                                                           (__toplevel_cons
                                                              'on
                                                              (__toplevel_cons 'this (__toplevel_cons 'fine (__toplevel_cons day-of-week ()))))))))))))))))))
   (let ((exp1 (__toplevel_cons 'list (__toplevel_cons 3 (__toplevel_cons 4 ()))))
         (res1a (__toplevel_cons 'list (__toplevel_cons (+ 1 2) (__toplevel_cons 4 ()))))
         (res1b (__toplevel_cons 'list (__toplevel_cons (+ 1 2) (__toplevel_cons 4 ()))))
         (exp2 5)
         (res2a (+ 2 3))
         (res2b (+ 2 3))
         (exp3 (__toplevel_cons
                 'Welcome
                 (__toplevel_cons
                    'to
                    (__toplevel_cons
                       'the
                       (__toplevel_cons
                          'FooBar
                          (__toplevel_cons
                             'system!
                             (__toplevel_cons
                                'We
                                (__toplevel_cons
                                   'hope
                                   (__toplevel_cons
                                      'you
                                      (__toplevel_cons
                                         'enjoy
                                         (__toplevel_cons
                                            'your
                                            (__toplevel_cons
                                               'visit
                                               (__toplevel_cons 'on (__toplevel_cons 'this (__toplevel_cons 'fine (__toplevel_cons 'Sunday ()))))))))))))))))
         (res3 (make-greeting))
         (exp4 (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (res4a (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (res4b (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (exp5 (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 12 ()))))
         (res5a (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons (* 3 4) ()))))
         (res5b (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons (* 3 4) ())))))
      (if (equal? exp1 res1a)
         (if (equal? exp1 res1b)
            (<change>
               (if (= exp2 res2a)
                  (if (= exp2 res2b)
                     (if (equal? exp3 res3)
                        (if (equal? exp4 res4a)
                           (if (equal? exp4 res4b)
                              (if (equal? exp5 res5a) (equal? exp5 res5b) #f)
                              #f)
                           #f)
                        #f)
                     #f)
                  #f)
               #f)
            (<change>
               #f
               (if (= exp2 res2a)
                  #f
                  (if (= exp2 res2b)
                     (if (equal? exp3 res3)
                        (if (equal? exp4 res4a)
                           (if (equal? exp4 res4b)
                              (if (not (equal? exp5 res5a))
                                 (equal? exp5 res5b)
                                 #f)
                              #f)
                           #f)
                        #f)
                     #f))))
         #f)))