; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 3
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (weird (lambda (x)
                  (if (<change> (= x 1) (not (= x 1)))
                     1
                     (if (even? x)
                        (weird (/ x 2))
                        (weird (+ (* 3 x) 1))))))
         (depth-weird (lambda (x)
                        (if (= x 1)
                           0
                           (if (even? x)
                              (+ 1 (depth-weird (/ x 2)))
                              (+ (depth-weird (+ (* 3 x) 1)) 1)))))
         (weird-table (lambda (min max)
                        (if (<change> (< min max) (not (< min max)))
                           (begin
                              (for-each output (list min "\t" (depth-weird min) "\n"))
                              (weird-table (+ min 1) max))
                           #f))))
   (weird-table 1 10)
   (if (= (weird 15) 1)
      (if (<change> (= (depth-weird 15) 17) (not (= (depth-weird 15) 17)))
         (equal?
            result
            (__toplevel_cons
               "\n"
               (__toplevel_cons
                  19
                  (__toplevel_cons
                     "\t"
                     (__toplevel_cons
                        9
                        (__toplevel_cons
                           "\n"
                           (__toplevel_cons
                              3
                              (__toplevel_cons
                                 "\t"
                                 (__toplevel_cons
                                    8
                                    (__toplevel_cons
                                       "\n"
                                       (__toplevel_cons
                                          16
                                          (__toplevel_cons
                                             "\t"
                                             (__toplevel_cons
                                                7
                                                (__toplevel_cons
                                                   "\n"
                                                   (__toplevel_cons
                                                      8
                                                      (__toplevel_cons
                                                         "\t"
                                                         (__toplevel_cons
                                                            6
                                                            (__toplevel_cons
                                                               "\n"
                                                               (__toplevel_cons
                                                                  5
                                                                  (__toplevel_cons
                                                                     "\t"
                                                                     (__toplevel_cons
                                                                        5
                                                                        (__toplevel_cons
                                                                           "\n"
                                                                           (__toplevel_cons
                                                                              2
                                                                              (__toplevel_cons
                                                                                 "\t"
                                                                                 (__toplevel_cons
                                                                                    4
                                                                                    (__toplevel_cons
                                                                                       "\n"
                                                                                       (__toplevel_cons
                                                                                          7
                                                                                          (__toplevel_cons
                                                                                             "\t"
                                                                                             (__toplevel_cons
                                                                                                3
                                                                                                (__toplevel_cons
                                                                                                   "\n"
                                                                                                   (__toplevel_cons
                                                                                                      1
                                                                                                      (__toplevel_cons
                                                                                                         "\t"
                                                                                                         (__toplevel_cons
                                                                                                            2
                                                                                                            (__toplevel_cons "\n" (__toplevel_cons 0 (__toplevel_cons "\t" (__toplevel_cons 1 ())))))))))))))))))))))))))))))))))))))
         #f)
      #f))