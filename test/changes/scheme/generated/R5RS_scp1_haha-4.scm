; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (hulp 2)
         (haha (lambda (x)
                 (let ((hulp (* x hulp)))
                    (output hulp))
                 (<change>
                    (output hulp)
                    (set! hulp 4))
                 (<change>
                    (set! hulp 4)
                    (output hulp)))))
   (haha 2)
   (haha 3)
   (equal? result (__toplevel_cons 4 (__toplevel_cons 12 (__toplevel_cons 2 (__toplevel_cons 4 ()))))))