; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((square (lambda (x)
                   (* x x)))
         (modulo-power (lambda (base exp n)
                         (if (= exp 0)
                            1
                            (if (odd? exp)
                               (modulo (* base (modulo-power base (- exp 1) n)) n)
                               (modulo (square (modulo-power base (/ exp 2) n)) n)))))
         (is-trivial-composite? (lambda (n)
                                  (<change>
                                     (let ((__or_res (= (modulo n 2) 0)))
                                        (if __or_res
                                           __or_res
                                           (let ((__or_res (= (modulo n 3) 0)))
                                              (if __or_res
                                                 __or_res
                                                 (let ((__or_res (= (modulo n 5) 0)))
                                                    (if __or_res
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
                                                                                  (if __or_res __or_res (= (modulo n 23) 0)))))))))))))))))
                                     ((lambda (x) x)
                                        (let ((__or_res (= (modulo n 2) 0)))
                                           (if __or_res
                                              __or_res
                                              (let ((__or_res (= (modulo n 3) 0)))
                                                 (if (<change> __or_res (not __or_res))
                                                    __or_res
                                                    (let ((__or_res (= (modulo n 5) 0)))
                                                       (if __or_res
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
                                                                                     (if __or_res __or_res (= (modulo n 23) 0)))))))))))))))))))))
         (is-fermat-prime? (lambda (n iterations)
                             (let ((__or_res (<= iterations 0)))
                                (if __or_res
                                   (<change>
                                      __or_res
                                      (let* ((byte-size (ceiling (/ (log n) (log 2))))
                                             (a (random byte-size)))
                                         (if (= (modulo-power a (- n 1) n) 1)
                                            (is-fermat-prime? n (- iterations 1))
                                            #f)))
                                   (<change>
                                      (let* ((byte-size (ceiling (/ (log n) (log 2))))
                                             (a (random byte-size)))
                                         (if (= (modulo-power a (- n 1) n) 1)
                                            (is-fermat-prime? n (- iterations 1))
                                            #f))
                                      __or_res)))))
         (generate-fermat-prime (lambda (byte-size iterations)
                                  (let ((n (random byte-size)))
                                     (if (if (not (is-trivial-composite? n)) (is-fermat-prime? n iterations) #f)
                                        n
                                        (generate-fermat-prime byte-size iterations)))))
         (iterations 10)
         (byte-size 15))
   (display "Generating prime...")
   (newline)
   (display (generate-fermat-prime byte-size iterations))
   (display " is prime with at least probability 1 - 1/2^")
   (display iterations)
   (<change>
      ()
      (display (generate-fermat-prime byte-size iterations)))
   (newline)
   (<change>
      (display " if it is not a Carmichael number.")
      (newline))
   (<change>
      (newline)
      (display " if it is not a Carmichael number.")))