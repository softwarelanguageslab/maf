; Changes:
; * removed: 0
; * added: 2
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((result ())
         (output (lambda (i)
                   (<change>
                      ()
                      result)
                   (set! result (cons i result))))
         (hulp 2)
         (haha (lambda (x)
                 (<change>
                    ()
                    (* x hulp))
                 (let ((hulp (* x hulp)))
                    (<change>
                       (output hulp)
                       ((lambda (x) x) (output hulp))))
                 (<change>
                    (output hulp)
                    (set! hulp 4))
                 (<change>
                    (set! hulp 4)
                    (output hulp)))))
   (haha 2)
   (haha 3)
   (<change>
      (equal? result (__toplevel_cons 4 (__toplevel_cons 12 (__toplevel_cons 2 (__toplevel_cons 4 ())))))
      ((lambda (x) x)
         (equal? result (__toplevel_cons 4 (__toplevel_cons 12 (__toplevel_cons 2 (__toplevel_cons 4 ()))))))))