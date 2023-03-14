; Changes:
; * removed: 0
; * added: 2
; * swaps: 2
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((s "abcdef")
         (grow (lambda ()
                 (set! s (string-append "123" s "456" s "789"))
                 (<change>
                    (set! s (string-append
                            (substring s (quotient (string-length s) 2) (string-length s))
                            (substring s 0 (+ 1 (quotient (string-length s) 2)))))
                    s)
                 (<change>
                    s
                    (set! s (string-append
                            (substring s (quotient (string-length s) 2) (string-length s))
                            (substring s 0 (+ 1 (quotient (string-length s) 2))))))))
         (trial (lambda (n)
                  (letrec ((__do_loop (lambda (i)
                                        (<change>
                                           ()
                                           s)
                                        (if (> (string-length s) n)
                                           (string-length s)
                                           (begin
                                              (grow)
                                              (<change>
                                                 (__do_loop (+ i 1))
                                                 ((lambda (x) x) (__do_loop (+ i 1)))))))))
                     (__do_loop 0))))
         (my-try (lambda (n)
                   (letrec ((__do_loop (lambda (i)
                                         (if (<change> (>= i 10) (not (>= i 10)))
                                            (string-length s)
                                            (begin
                                               (set! s "abcdef")
                                               (<change>
                                                  (trial n)
                                                  (__do_loop (+ i 1)))
                                               (<change>
                                                  (__do_loop (+ i 1))
                                                  (trial n)))))))
                      (__do_loop 0)))))
   (<change>
      ()
      (display =))
   (= (my-try 500000) 524278))