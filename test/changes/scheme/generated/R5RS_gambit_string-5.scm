; Changes:
; * removed: 1
; * added: 3
; * swaps: 0
; * negated predicates: 0
(letrec ((s "abcdef")
         (grow (lambda ()
                 (set! s (string-append "123" s "456" s "789"))
                 (set! s (string-append
                         (substring s (quotient (string-length s) 2) (string-length s))
                         (substring s 0 (+ 1 (quotient (string-length s) 2)))))
                 s))
         (trial (lambda (n)
                  (letrec ((__do_loop (lambda (i)
                                        (if (> (string-length s) n)
                                           (string-length s)
                                           (begin
                                              (<change>
                                                 (grow)
                                                 ())
                                              (__do_loop (+ i 1)))))))
                     (__do_loop 0))))
         (my-try (lambda (n)
                   (<change>
                      ()
                      trial)
                   (<change>
                      ()
                      i)
                   (letrec ((__do_loop (lambda (i)
                                         (if (>= i 10)
                                            (string-length s)
                                            (begin
                                               (set! s "abcdef")
                                               (trial n)
                                               (__do_loop (+ i 1)))))))
                      (<change>
                         ()
                         (__do_loop 0))
                      (__do_loop 0)))))
   (= (my-try 500000) 524278))