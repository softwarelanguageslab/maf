; Changes:
; * removed: 0
; * added: 2
; * swaps: 1
; * negated predicates: 0
(letrec ((s "abcdef")
         (grow (lambda ()
                 (set! s (string-append "123" s "456" s "789"))
                 (<change>
                    ()
                    (display 0))
                 (set! s (string-append
                         (substring s (quotient (string-length s) 2) (string-length s))
                         (substring s 0 (+ 1 (quotient (string-length s) 2)))))
                 s))
         (trial (lambda (n)
                  (letrec ((__do_loop (lambda (i)
                                        (if (> (string-length s) n)
                                           (string-length s)
                                           (begin
                                              (grow)
                                              (__do_loop (+ i 1)))))))
                     (__do_loop 0))))
         (my-try (lambda (n)
                   (letrec ((__do_loop (lambda (i)
                                         (if (>= i 10)
                                            (string-length s)
                                            (begin
                                               (<change>
                                                  ()
                                                  n)
                                               (set! s "abcdef")
                                               (<change>
                                                  (trial n)
                                                  (__do_loop (+ i 1)))
                                               (<change>
                                                  (__do_loop (+ i 1))
                                                  (trial n)))))))
                      (__do_loop 0)))))
   (= (my-try 500000) 524278))