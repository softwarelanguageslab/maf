; Changes:
; * removed: 0
; * added: 2
; * swaps: 2
; * negated predicates: 0
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (linebreak (lambda ()
                      (set! result (cons 'linebreak result))))
         (create-counter (lambda ()
                           (let ((value 0))
                              (letrec ((reset (lambda ()
                                                (set! value 0)
                                                'ok))
                                       (next (lambda ()
                                               (set! value (+ 1 value))
                                               'ok))
                                       (increase (lambda (x)
                                                   (set! value (+ value x))))
                                       (dispatch (lambda (msg)
                                                   (if (eq? msg 'reset)
                                                      reset
                                                      (if (eq? msg 'next)
                                                         next
                                                         (if (eq? msg 'read)
                                                            value
                                                            (if (eq? msg 'increase)
                                                               increase
                                                               (error "wrong message: " msg))))))))
                                 dispatch))))
         (make-scorebord (lambda ()
                           (let ((c-home (create-counter))
                                 (c-visit (create-counter)))
                              (letrec ((reset (lambda ()
                                                ((c-home 'reset))
                                                ((c-visit 'reset))
                                                'ok))
                                       (read (lambda ()
                                               (let ((c1 (c-home 'read))
                                                     (c2 (c-visit 'read)))
                                                  (output c1)
                                                  (output "-")
                                                  (<change>
                                                     ()
                                                     c1)
                                                  (<change>
                                                     (output c2)
                                                     (linebreak))
                                                  (<change>
                                                     (linebreak)
                                                     (output c2))
                                                  'ok)))
                                       (score (lambda (team n)
                                                (<change>
                                                   ()
                                                   team)
                                                (if (not (let ((__or_res (= n 1))) (if __or_res __or_res (let ((__or_res (= n 2))) (if __or_res __or_res (= n 3))))))
                                                   (begin
                                                      (linebreak)
                                                      (output "De score kan slechts 1, 2 of 3 zijn!")
                                                      (linebreak)
                                                      'ok)
                                                   (if (eq? team 'home)
                                                      (begin
                                                         ((c-home 'increase) n)
                                                         'ok)
                                                      (if (eq? team 'visit)
                                                         (begin
                                                            ((c-visit 'increase) n)
                                                            'ok)
                                                         (error "wrong team: " team))))))
                                       (dispatch (lambda (msg)
                                                   (if (eq? msg 'reset)
                                                      reset
                                                      (if (eq? msg 'read)
                                                         read
                                                         (if (eq? msg 'score)
                                                            score
                                                            (error "wrong message: " msg)))))))
                                 dispatch))))
         (bord (make-scorebord)))
   ((bord 'read))
   (<change>
      ((bord 'score) 'home 2)
      ((bord 'read)))
   (<change>
      ((bord 'read))
      ((bord 'score) 'home 2))
   ((bord 'score) 'visit 5)
   ((bord 'read))
   ((bord 'reset))
   ((bord 'read))
   (equal?
      result
      (__toplevel_cons
         'linebreak
         (__toplevel_cons
            0
            (__toplevel_cons
               "-"
               (__toplevel_cons
                  0
                  (__toplevel_cons
                     'linebreak
                     (__toplevel_cons
                        0
                        (__toplevel_cons
                           "-"
                           (__toplevel_cons
                              2
                              (__toplevel_cons
                                 'linebreak
                                 (__toplevel_cons
                                    "De score kan slechts 1, 2 of 3 zijn!"
                                    (__toplevel_cons
                                       'linebreak
                                       (__toplevel_cons
                                          'linebreak
                                          (__toplevel_cons
                                             0
                                             (__toplevel_cons
                                                "-"
                                                (__toplevel_cons
                                                   2
                                                   (__toplevel_cons 'linebreak (__toplevel_cons 0 (__toplevel_cons "-" (__toplevel_cons 0 ())))))))))))))))))))))