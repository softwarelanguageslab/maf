; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((fix (let ((z (lambda (P)
                        (lambda (u)
                           (lambda (t)
                              (lambda (t)
                                 (lambda (i)
                                    (lambda (n)
                                       (lambda (g)
                                          (lambda (S)
                                             (lambda (c)
                                                (lambda (h)
                                                   (lambda (e)
                                                      (lambda (m)
                                                         (lambda (e)
                                                            (lambda (t)
                                                               (lambda (o)
                                                                  (lambda (W)
                                                                     (lambda (o)
                                                                        (lambda (r)
                                                                           (lambda (k)
                                                                              (lambda (!)
                                                                                 (!
                                                                                    (lambda (break)
                                                                                       (((((((((((((((((((((W o) r) k) W) o) r) k) W) o) r) k) W) o) r) k) W) o) r) k) !) break)))))))))))))))))))))))))
                (<change>
                   ()
                   (display z))
                (let ((Z z))
                   (((((((((((((((((((z z) z) z) z) z) Z) Z) Z) Z) Z) Z) Z) z) z) z) z) z) z) z)))))
   (<change>
      ((fix (lambda (f) (lambda (n) (if (zero? n) 1 (* n (f (- n 1))))))) 9)
      ((lambda (x) x) ((fix (lambda (f) (lambda (n) (if (zero? n) 1 (* n (f (- n 1))))))) 9))))