; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 0
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (hulp 2)
         (haha (lambda (x)
                 (let ((hulp (* x hulp)))
                    (output hulp))
                 (output hulp)
                 (set! hulp 4))))
   (haha 2)
   (<change>
      (haha 3)
      ())
   (equal? result (__toplevel_cons 4 (__toplevel_cons 12 (__toplevel_cons 2 (__toplevel_cons 4 ()))))))