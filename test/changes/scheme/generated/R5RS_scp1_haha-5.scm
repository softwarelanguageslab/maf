; Changes:
; * removed: 1
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 4
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (hulp 2)
         (haha (lambda (x)
                 (<change>
                    (let ((hulp (* x hulp)))
                       (output hulp))
                    ())
                 (<change>
                    ()
                    (display output))
                 (<change>
                    (output hulp)
                    ((lambda (x) x) (output hulp)))
                 (set! hulp 4))))
   (<change>
      (haha 2)
      ((lambda (x) x) (haha 2)))
   (<change>
      (haha 3)
      ((lambda (x) x) (haha 3)))
   (<change>
      (equal? result (__toplevel_cons 4 (__toplevel_cons 12 (__toplevel_cons 2 (__toplevel_cons 4 ())))))
      ((lambda (x) x)
         (equal? result (__toplevel_cons 4 (__toplevel_cons 12 (__toplevel_cons 2 (__toplevel_cons 4 ()))))))))