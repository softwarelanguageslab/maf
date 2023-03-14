; Changes:
; * removed: 1
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 3
(letrec ((result ())
         (output (lambda (i)
                   (<change>
                      (set! result (cons i result))
                      ((lambda (x) x) (set! result (cons i result))))))
         (linebreak (lambda ()
                      (set! result (cons 'linebreak result))))
         (create-counter (lambda ()
                           (<change>
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
                                    dispatch))
                              ((lambda (x) x)
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
                                       dispatch))))))
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
                                                  (output c2)
                                                  (linebreak)
                                                  'ok)))
                                       (score (lambda (team n)
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
   ((bord 'score) 'home 2)
   (<change>
      ((bord 'read))
      ((lambda (x) x) ((bord 'read))))
   ((bord 'score) 'visit 5)
   (<change>
      ((bord 'read))
      ((bord 'reset)))
   (<change>
      ((bord 'reset))
      ((bord 'read)))
   (<change>
      ((bord 'read))
      ())
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