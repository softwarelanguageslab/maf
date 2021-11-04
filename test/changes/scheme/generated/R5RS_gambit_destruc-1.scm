; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 2
; * swapped branches: 1
; * calls to id fun: 2
(letrec ((append-to-tail! (lambda (x y)
                            (if (null? x)
                               y
                               ((letrec ((loop (lambda (a b)
                                                (if (null? b)
                                                   (begin
                                                      (set-cdr! a y)
                                                      x)
                                                   (loop b (cdr b))))))
                                  loop)
                                  x
                                  (cdr x)))))
         (destructive (lambda (n m)
                        (<change>
                           ()
                           (display quotient))
                        (let ((l (letrec ((__do_loop (lambda (i a)
                                                      (if (= i 0) a (__do_loop (- i 1) (cons () a))))))
                                   (__do_loop 10 ()))))
                           (letrec ((__do_loop (lambda (i)
                                                 (if (= i 0)
                                                    l
                                                    (begin
                                                       (if (null? (car l))
                                                          (letrec ((__do_loop (lambda (l)
                                                                                (if (null? l)
                                                                                   (<change>
                                                                                      #f
                                                                                      (begin
                                                                                         (if (null? (car l)) (set-car! l (cons () ())) #f)
                                                                                         (append-to-tail!
                                                                                            (car l)
                                                                                            (letrec ((__do_loop (lambda (j a)
                                                                                                                  ((lambda (x) x) (if (= j 0) a (__do_loop (- j 1) (cons () a)))))))
                                                                                               (__do_loop m ())))
                                                                                         (__do_loop (cdr l))))
                                                                                   (<change>
                                                                                      (begin
                                                                                         (if (null? (car l)) (set-car! l (cons () ())) #f)
                                                                                         (append-to-tail!
                                                                                            (car l)
                                                                                            (letrec ((__do_loop (lambda (j a)
                                                                                                                  (if (= j 0) a (__do_loop (- j 1) (cons () a))))))
                                                                                               (__do_loop m ())))
                                                                                         (__do_loop (cdr l)))
                                                                                      #f)))))
                                                             (__do_loop l))
                                                          (letrec ((__do_loop (lambda (l1 l2)
                                                                                (if (null? l2)
                                                                                   #f
                                                                                   (begin
                                                                                      (set-cdr!
                                                                                         (letrec ((__do_loop (lambda (j a)
                                                                                                               (if (zero? j)
                                                                                                                  a
                                                                                                                  (begin
                                                                                                                     (<change>
                                                                                                                        (set-car! a i)
                                                                                                                        ((lambda (x) x) (set-car! a i)))
                                                                                                                     (__do_loop (- j 1) (cdr a)))))))
                                                                                            (__do_loop (quotient (length (car l2)) 2) (car l2)))
                                                                                         (let ((n (quotient (length (car l1)) 2)))
                                                                                            (if (<change> (= n 0) (not (= n 0)))
                                                                                               (begin
                                                                                                  (<change>
                                                                                                     (set-car! l1 ())
                                                                                                     (car l1))
                                                                                                  (<change>
                                                                                                     (car l1)
                                                                                                     (set-car! l1 ())))
                                                                                               (letrec ((__do_loop (lambda (j a)
                                                                                                                     (if (<change> (= j 1) (not (= j 1)))
                                                                                                                        (let ((x (cdr a)))
                                                                                                                           (set-cdr! a ())
                                                                                                                           x)
                                                                                                                        (begin
                                                                                                                           (set-car! a i)
                                                                                                                           (__do_loop (- j 1) (cdr a)))))))
                                                                                                  (__do_loop n (car l1))))))
                                                                                      (__do_loop (cdr l1) (cdr l2)))))))
                                                             (__do_loop l (cdr l))))
                                                       (__do_loop (- i 1)))))))
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