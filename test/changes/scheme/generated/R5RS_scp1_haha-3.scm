; Changes:
; * removed: 1
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((result ())
         (output (lambda (i)
                   (<change>
                      (set! result (cons i result))
                      ((lambda (x) x) (set! result (cons i result))))))
         (hulp 2)
         (haha (lambda (x)
                 (<change>
                    (let ((hulp (* x hulp)))
                       (output hulp))
                    ())
                 (<change>
                    (output hulp)
                    (set! hulp 4))
                 (<change>
                    (set! hulp 4)
                    (output hulp)))))
   (haha 2)
   (haha 3)
   (equal? result (__toplevel_cons 4 (__toplevel_cons 12 (__toplevel_cons 2 (__toplevel_cons 4 ()))))))