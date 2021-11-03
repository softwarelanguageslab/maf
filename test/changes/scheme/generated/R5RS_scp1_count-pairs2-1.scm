; Changes:
; * removed: 0
; * added: 2
; * swaps: 2
; * negated predicates: 0
(letrec ((count-pairs (lambda (lst)
                        (let ((path ()))
                           (letrec ((count (lambda (current)
                                             (if (null? current)
                                                0
                                                (if (not (pair? current))
                                                   0
                                                   (if (memq current path)
                                                      0
                                                      (begin
                                                         (<change>
                                                            (set! path (cons current path))
                                                            (+ 1 (count (car current)) (count (cdr current))))
                                                         (<change>
                                                            (+ 1 (count (car current)) (count (cdr current)))
                                                            (set! path (cons current path))))))))))
                              (count lst)))))
         (ret3 (cons 'a (cons 'b (cons 'c ()))))
         (ret4 (let ((last (cons 'c ())))
                 (<change>
                    ()
                    'b)
                 (<change>
                    ()
                    (cons 'b last))
                 (cons last (cons 'b last))))
         (ret7 (let* ((last (cons 'c ()))
                     (middle (cons last last)))
                 (cons middle middle)))
         (retno (let* ((last (cons 'c ()))
                      (lst (cons 'a (cons 'b last))))
                  (<change>
                     (set-cdr! last lst)
                     lst)
                  (<change>
                     lst
                     (set-cdr! last lst)))))
   (= 3 (count-pairs ret3) (count-pairs ret4) (count-pairs ret7) (count-pairs retno)))