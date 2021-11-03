; Changes:
; * removed: 1
; * added: 2
; * swaps: 1
; * negated predicates: 0
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (hulp 2)
         (haha (lambda (x)
                 (<change>
                    ()
                    (* x hulp))
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
      ())
   (<change>
      ()
      haha)
   (haha 3)
   (equal? result (__toplevel_cons 4 (__toplevel_cons 12 (__toplevel_cons 2 (__toplevel_cons 4 ()))))))