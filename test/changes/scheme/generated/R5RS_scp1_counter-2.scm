; Changes:
; * removed: 1
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (count1 (lambda (x)
                   (if (= 0 x)
                      (display x)
                      (begin
                         (display x)
                         (<change>
                            (count1 (- x 1))
                            ((lambda (x) x) (count1 (- x 1))))))))
         (count2 (lambda (x)
                   (if (= 0 x)
                      (display x)
                      (begin
                         (count2 (- x 1))
                         (display x))))))
   (<change>
      (count1 4)
      ())
   (<change>
      (count2 4)
      (equal?
         result
         (__toplevel_cons
            4
            (__toplevel_cons
               3
               (__toplevel_cons
                  2
                  (__toplevel_cons
                     1
                     (__toplevel_cons
                        0
                        (__toplevel_cons
                           0
                           (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 ()))))))))))))
   (<change>
      (equal?
         result
         (__toplevel_cons
            4
            (__toplevel_cons
               3
               (__toplevel_cons
                  2
                  (__toplevel_cons
                     1
                     (__toplevel_cons
                        0
                        (__toplevel_cons
                           0
                           (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 ())))))))))))
      (count2 4)))