; Changes:
; * removed: 1
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((permutations (lambda (x)
                         (let ((x x)
                               (perms (list x)))
                            (letrec ((P (lambda (n)
                                          (if (> n 1)
                                             (letrec ((__do_loop (lambda (j)
                                                                   (if (<change> (zero? j) (not (zero? j)))
                                                                      (P (- n 1))
                                                                      (begin
                                                                         (P (- n 1))
                                                                         (F n)
                                                                         (__do_loop (- j 1)))))))
                                                (__do_loop (- n 1)))
                                             #f)))
                                     (F (lambda (n)
                                          (<change>
                                             (set! x (revloop x n (list-tail x n)))
                                             ())
                                          (set! perms (cons x perms))))
                                     (revloop (lambda (x n y)
                                                (if (zero? n)
                                                   y
                                                   (revloop (cdr x) (- n 1) (cons (car x) y)))))
                                     (list-tail (lambda (x n)
                                                  (if (zero? n) x (list-tail (cdr x) (- n 1))))))
                               (P (length x))
                               perms))))
         (sumlists (lambda (x)
                     (letrec ((__do_loop (lambda (x sum)
                                           (<change>
                                              ()
                                              (display cdr))
                                           (if (null? x)
                                              sum
                                              (__do_loop
                                                 (cdr x)
                                                 (letrec ((__do_loop (lambda (y sum)
                                                                       (if (null? y)
                                                                          sum
                                                                          (__do_loop (cdr y) (+ sum (car y)))))))
                                                    (__do_loop (car x) sum)))))))
                        (__do_loop x 0))))
         (one..n (lambda (n)
                   (letrec ((__do_loop (lambda (n p)
                                         (if (zero? n) p (__do_loop (- n 1) (cons n p))))))
                      (<change>
                         (__do_loop n ())
                         ((lambda (x) x) (__do_loop n ()))))))
         (factorial (lambda (n)
                      (if (= n 0) 1 (* n (factorial (- n 1)))))))
   (= (sumlists (permutations (one..n 9))) (* (quotient (* 9 (+ 9 1)) 2) (factorial 9))))