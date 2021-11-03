; Changes:
; * removed: 1
; * added: 0
; * swaps: 1
; * negated predicates: 1
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
                             (tail-rec-aux 0 n))
                          (<change>
                             (tail-rec-aux 0 n)
                             (set! sum 0))
                          sum))
         (do-loop (lambda (n)
                    (<change>
                       (set! sum 0)
                       ())
                    (letrec ((__do_loop (lambda (i)
                                          (if (<change> (>= i n) (not (>= i n)))
                                             sum
                                             (begin
                                                (set! sum (+ sum 1))
                                                (__do_loop (+ i 1)))))))
                       (__do_loop 0)))))
   (= (do-loop 1000) 1000))