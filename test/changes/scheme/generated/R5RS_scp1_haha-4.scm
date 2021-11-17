; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (hulp 2)
         (haha (lambda (x)
                 (<change>
                    (let ((hulp (* x hulp)))
                       (output hulp))
                    (output hulp))
                 (<change>
                    (output hulp)
                    (let ((hulp (* x hulp)))
                       (output hulp)))
                 (set! hulp 4))))
   (<change>
      (haha 2)
      ((lambda (x) x) (haha 2)))
   (haha 3)
   (equal? result (__toplevel_cons 4 (__toplevel_cons 12 (__toplevel_cons 2 (__toplevel_cons 4 ()))))))