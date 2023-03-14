; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((fix (let ((z (lambda (P)
                        (<change>
                           ()
                           (lambda (o)
                              (lambda (r)
                                 (lambda (k)
                                    (lambda (!)
                                       (((((((((((W o) r) k) W) o) r) k) W) o) r) k)
                                       (!
                                          (lambda (break)
                                             (((((((((((((((((((((W o) r) k) W) o) r) k) W) o) r) k) W) o) r) k) W) o) r) k) !) break))))))))
                        (lambda (u)
                           (lambda (t)
                              (lambda (t)
                                 (<change>
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
                                                                                          (((((((((((((((((((((W o) r) k) W) o) r) k) W) o) r) k) W) o) r) k) W) o) r) k) !) break)))))))))))))))))))
                                    ((lambda (x) x)
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
                                                                                             (((((((((((((((((((((W o) r) k) W) o) r) k) W) o) r) k) W) o) r) k) W) o) r) k) !) break)))))))))))))))))))))))))))
                (let ((Z z))
                   (((((((((((((((((((z z) z) z) z) z) Z) Z) Z) Z) Z) Z) Z) z) z) z) z) z) z) z)))))
   ((fix (lambda (f) (lambda (n) (if (zero? n) 1 (* n (f (- n 1))))))) 9))