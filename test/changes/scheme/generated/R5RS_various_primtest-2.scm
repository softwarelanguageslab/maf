; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((square (lambda (x)
                   (* x x)))
         (modulo-power (lambda (base exp n)
                         (if (= exp 0)
                            (<change>
                               1
                               (if (odd? exp)
                                  (modulo (* base (modulo-power base (- exp 1) n)) n)
                                  (modulo (square (modulo-power base (/ exp 2) n)) n)))
                            (<change>
                               (if (odd? exp)
                                  (modulo (* base (modulo-power base (- exp 1) n)) n)
                                  (modulo (square (modulo-power base (/ exp 2) n)) n))
                               1))))
         (is-trivial-composite? (lambda (n)
                                  (let ((__or_res (= (modulo n 2) 0)))
                                     (if __or_res
                                        __or_res
                                        (let ((__or_res (= (modulo n 3) 0)))
                                           (if __or_res
                                              __or_res
                                              (let ((__or_res (= (modulo n 5) 0)))
                                                 (if (<change> __or_res (not __or_res))
                                                    __or_res
                                                    (let ((__or_res (= (modulo n 7) 0)))
                                                       (if __or_res
                                                          __or_res
                                                          (let ((__or_res (= (modulo n 11) 0)))
                                                             (if __or_res
                                                                __or_res
                                                                (let ((__or_res (= (modulo n 13) 0)))
                                                                   (if __or_res
                                                                      __or_res
                                                                      (let ((__or_res (= (modulo n 17) 0)))
                                                                         (if __or_res
                                                                            __or_res
                                                                            (let ((__or_res (= (modulo n 19) 0)))
                                                                               (if __or_res __or_res (= (modulo n 23) 0)))))))))))))))))))
         (is-fermat-prime? (lambda (n iterations)
                             (let ((__or_res (<= iterations 0)))
                                (if __or_res
                                   __or_res
                                   (let* ((byte-size (ceiling (/ (log n) (log 2))))
                                          (a (random byte-size)))
                                      (if (= (modulo-power a (- n 1) n) 1)
                                         (is-fermat-prime? n (- iterations 1))
                                         #f))))))
         (generate-fermat-prime (lambda (byte-size iterations)
                                  (let ((n (random byte-size)))
                                     (if (if (not (is-trivial-composite? n)) (is-fermat-prime? n iterations) #f)
                                        n
                                        (generate-fermat-prime byte-size iterations)))))
         (iterations 10)
         (byte-size 15))
   (generate-fermat-prime byte-size iterations))