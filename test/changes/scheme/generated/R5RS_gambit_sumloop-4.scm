; Changes:
; * removed: 0
; * added: 2
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((sum 0)
         (tail-rec-aux (lambda (i n)
                         (<change>
                            ()
                            (if (< i n)
                               (begin
                                  (tail-rec-aux (+ i 1) n)
                                  (set! sum (+ sum 1)))
                               sum))
                         (if (< i n)
                            (begin
                               (set! sum (+ sum 1))
                               (tail-rec-aux (+ i 1) n))
                            sum)))
         (tail-rec-loop (lambda (n)
                          (set! sum 0)
                          (tail-rec-aux 0 n)
                          sum))
         (do-loop (lambda (n)
                    (set! sum 0)
                    (letrec ((__do_loop (lambda (i)
                                          (if (>= i n)
                                             sum
                                             (begin
                                                (set! sum (+ sum 1))
                                                (__do_loop (+ i 1)))))))
                       (__do_loop 0)))))
   (<change>
      ()
      (do-loop 1000))
   (= (do-loop 1000) 1000))