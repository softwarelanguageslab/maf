; Changes:
; * removed: 1
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((s "abcdef")
         (grow (lambda ()
                 (set! s (string-append "123" s "456" s "789"))
                 (<change>
                    (set! s (string-append
                            (substring s (quotient (string-length s) 2) (string-length s))
                            (substring s 0 (+ 1 (quotient (string-length s) 2)))))
                    ())
                 s))
         (trial (lambda (n)
                  (<change>
                     (letrec ((__do_loop (lambda (i)
                                           (if (> (string-length s) n)
                                              (string-length s)
                                              (begin
                                                 (grow)
                                                 (__do_loop (+ i 1)))))))
                        (__do_loop 0))
                     ((lambda (x) x)
                        (letrec ((__do_loop (lambda (i)
                                              (if (> (string-length s) n)
                                                 (string-length s)
                                                 (begin
                                                    (<change>
                                                       (grow)
                                                       (__do_loop (+ i 1)))
                                                    (<change>
                                                       (__do_loop (+ i 1))
                                                       (grow)))))))
                           (__do_loop 0))))))
         (my-try (lambda (n)
                   (<change>
                      (letrec ((__do_loop (lambda (i)
                                            (if (>= i 10)
                                               (string-length s)
                                               (begin
                                                  (set! s "abcdef")
                                                  (trial n)
                                                  (__do_loop (+ i 1)))))))
                         (__do_loop 0))
                      ((lambda (x) x)
                         (letrec ((__do_loop (lambda (i)
                                               (<change>
                                                  ()
                                                  +)
                                               (if (>= i 10)
                                                  (string-length s)
                                                  (begin
                                                     (set! s "abcdef")
                                                     (trial n)
                                                     (__do_loop (+ i 1)))))))
                            (__do_loop 0)))))))
   (= (my-try 500000) 524278))