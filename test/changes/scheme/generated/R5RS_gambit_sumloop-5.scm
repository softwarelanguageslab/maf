; Changes:
; * removed: 0
; * added: 0
; * swaps: 2
; * negated predicates: 0
(letrec ((sum 0)
         (tail-rec-aux (lambda (i n)
                         (if (< i n)
                            (begin
                               (<change>
                                  (set! sum (+ sum 1))
                                  (tail-rec-aux (+ i 1) n))
                               (<change>
                                  (tail-rec-aux (+ i 1) n)
                                  (set! sum (+ sum 1))))
                            sum)))
         (tail-rec-loop (lambda (n)
                          (set! sum 0)
                          (tail-rec-aux 0 n)
                          sum))
         (do-loop (lambda (n)
                    (<change>
                       (set! sum 0)
                       (letrec ((__do_loop (lambda (i)
                                             (if (>= i n)
                                                sum
                                                (begin
                                                   (set! sum (+ sum 1))
                                                   (__do_loop (+ i 1)))))))
                          (__do_loop 0)))
                    (<change>
                       (letrec ((__do_loop (lambda (i)
                                             (if (>= i n)
                                                sum
                                                (begin
                                                   (set! sum (+ sum 1))
                                                   (__do_loop (+ i 1)))))))
                          (__do_loop 0))
                       (set! sum 0)))))
   (= (do-loop 1000) 1000))