; Changes:
; * removed: 0
; * added: 2
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((count-pairs (lambda (lst)
                        (let ((path ()))
                           (<change>
                              ()
                              (lambda (current)
                                 (if (null? current)
                                    0
                                    (if (not (pair? current))
                                       0
                                       (if (memq current path)
                                          0
                                          (begin
                                             (display +)
                                             (+ 1 (count (car current)) (count (cdr current)))
                                             (set! path (cons current path))))))))
                           (letrec ((count (lambda (current)
                                             (if (null? current)
                                                0
                                                (if (not (pair? current))
                                                   0
                                                   (if (memq current path)
                                                      (<change>
                                                         0
                                                         (begin
                                                            (set! path (cons current path))
                                                            (+ 1 (count (car current)) (count (cdr current)))))
                                                      (<change>
                                                         (begin
                                                            (set! path (cons current path))
                                                            (+ 1 (count (car current)) (count (cdr current))))
                                                         0)))))))
                              (count lst)))))
         (ret3 (cons 'a (cons 'b (cons 'c ()))))
         (ret4 (let ((last (cons 'c ())))
                 (cons last (cons 'b last))))
         (ret7 (let* ((last (cons 'c ()))
                     (middle (cons last last)))
                 (cons middle middle)))
         (retno (let* ((last (cons 'c ()))
                      (lst (cons 'a (cons 'b last))))
                  (set-cdr! last lst)
                  lst)))
   (= 3 (count-pairs ret3) (count-pairs ret4) (count-pairs ret7) (count-pairs retno)))