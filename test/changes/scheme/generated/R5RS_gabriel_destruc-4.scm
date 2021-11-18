; Changes:
; * removed: 1
; * added: 0
; * swaps: 2
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((append-to-tail! (lambda (x y)
                            @sensitivity:FA
                            (if (null? x)
                               y
                               ((letrec ((loop (lambda (a b)
                                                @sensitivity:FA
                                                (if (null? b)
                                                   (begin
                                                      (set-cdr! a y)
                                                      x)
                                                   (loop b (cdr b))))))
                                  loop)
                                  x
                                  (cdr x)))))
         (destructive (lambda (n m)
                        @sensitivity:FA
                        (let ((l (letrec ((__do_loop (lambda (i a)
                                                      (<change>
                                                         @sensitivity:FA
                                                         (if (= i 0) a (__do_loop (- i 1) (cons () a))))
                                                      (<change>
                                                         (if (= i 0) a (__do_loop (- i 1) (cons () a)))
                                                         @sensitivity:FA))))
                                   (<change>
                                      (__do_loop 10 ())
                                      ((lambda (x) x) (__do_loop 10 ()))))))
                           (letrec ((__do_loop (lambda (i)
                                                 (<change>
                                                    @sensitivity:No
                                                    (if (= i 0) l (begin (__do_loop (- i 1)))))
                                                 (<change>
                                                    (if (= i 0)
                                                       l
                                                       (begin
                                                          (if (null? (car l))
                                                             (letrec ((__do_loop (lambda (l)
                                                                                   @sensitivity:FA
                                                                                   (if (null? l)
                                                                                      #f
                                                                                      (begin
                                                                                         (if (null? (car l)) (set-car! l (cons () ())) #f)
                                                                                         (append-to-tail!
                                                                                            (car l)
                                                                                            (letrec ((__do_loop (lambda (j a)
                                                                                                                  @sensitivity:No
                                                                                                                  (if (= j 0) a (__do_loop (- j 1) (cons () a))))))
                                                                                               (__do_loop m ())))
                                                                                         (__do_loop (cdr l)))))))
                                                                (__do_loop l))
                                                             (letrec ((__do_loop (lambda (l1 l2)
                                                                                   @sensitivity:FA
                                                                                   (if (null? l2)
                                                                                      #f
                                                                                      (begin
                                                                                         (set-cdr!
                                                                                            (letrec ((__do_loop (lambda (j a)
                                                                                                                  @sensitivity:FA
                                                                                                                  (if (zero? j)
                                                                                                                     a
                                                                                                                     (begin
                                                                                                                        (set-car! a i)
                                                                                                                        (__do_loop (- j 1) (cdr a)))))))
                                                                                               (__do_loop (quotient (length (car l2)) 2) (car l2)))
                                                                                            (let ((n (quotient (length (car l1)) 2)))
                                                                                               (if (= n 0)
                                                                                                  (begin
                                                                                                     (set-car! l1 ())
                                                                                                     (car l1))
                                                                                                  (letrec ((__do_loop (lambda (j a)
                                                                                                                        @sensitivity:FA
                                                                                                                        (if (= j 1)
                                                                                                                           (let ((x (cdr a)))
                                                                                                                              (set-cdr! a ())
                                                                                                                              x)
                                                                                                                           (begin
                                                                                                                              (set-car! a i)
                                                                                                                              (__do_loop (- j 1) (cdr a)))))))
                                                                                                     (__do_loop n (car l1))))))
                                                                                         (__do_loop (cdr l1) (cdr l2)))))))
                                                                (__do_loop l (cdr l))))
                                                          (__do_loop (- i 1))))
                                                    @sensitivity:No))))
                              (__do_loop n))))))
   (equal?
      (destructive 600 50)
      (__toplevel_cons
         (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))
         (__toplevel_cons
            (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))
            (__toplevel_cons
               (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ()))))
               (__toplevel_cons
                  (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ()))))
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons
                                          1
                                          (__toplevel_cons
                                             1
                                             (__toplevel_cons
                                                1
                                                (__toplevel_cons
                                                   1
                                                   (__toplevel_cons
                                                      1
                                                      (__toplevel_cons
                                                         1
                                                         (__toplevel_cons
                                                            1
                                                            (__toplevel_cons
                                                               1
                                                               (__toplevel_cons
                                                                  1
                                                                  (__toplevel_cons
                                                                     1
                                                                     (__toplevel_cons
                                                                        1
                                                                        (__toplevel_cons
                                                                           1
                                                                           (__toplevel_cons
                                                                              1
                                                                              (__toplevel_cons
                                                                                 1
                                                                                 (__toplevel_cons
                                                                                    2
                                                                                    (__toplevel_cons
                                                                                       2
                                                                                       (__toplevel_cons 2 (__toplevel_cons 2 (__toplevel_cons 2 (__toplevel_cons 3 ())))))))))))))))))))))
                                    ()))))))))))))