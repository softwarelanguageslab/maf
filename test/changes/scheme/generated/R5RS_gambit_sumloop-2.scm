; Changes:
; * removed: 1
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((sum 0)
         (tail-rec-aux (lambda (i n)
                         (if (< i n)
                            (begin
                               (set! sum (+ sum 1))
                               (tail-rec-aux (+ i 1) n))
                            sum)))
         (tail-rec-loop (lambda (n)
                          (<change>
                             (set! sum 0)
                             ((lambda (x) x) (set! sum 0)))
                          (<change>
                             (tail-rec-aux 0 n)
                             ())
                          sum))
         (do-loop (lambda (n)
                    (set! sum 0)
                    (<change>
                       (letrec ((__do_loop (lambda (i)
                                             (if (>= i n)
                                                sum
                                                (begin
                                                   (set! sum (+ sum 1))
                                                   (__do_loop (+ i 1)))))))
                          (__do_loop 0))
                       ((lambda (x) x)
                          (letrec ((__do_loop (lambda (i)
                                                (if (>= i n)
                                                   sum
                                                   (begin
                                                      (<change>
                                                         (set! sum (+ sum 1))
                                                         (__do_loop (+ i 1)))
                                                      (<change>
                                                         (__do_loop (+ i 1))
                                                         (set! sum (+ sum 1))))))))
                             (__do_loop 0)))))))
   (= (do-loop 1000) 1000))