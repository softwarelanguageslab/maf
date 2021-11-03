; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((count-pairs (lambda (lst)
                        (let ((path ()))
                           (letrec ((count (lambda (current)
                                             (<change>
                                                (if (null? current)
                                                   0
                                                   (if (not (pair? current))
                                                      0
                                                      (if (memq current path)
                                                         0
                                                         (begin
                                                            (set! path (cons current path))
                                                            (+ 1 (count (car current)) (count (cdr current)))))))
                                                ((lambda (x) x)
                                                   (if (null? current)
                                                      0
                                                      (if (<change> (not (pair? current)) (not (not (pair? current))))
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
                                                               0)))))))))
                              (count lst)))))
         (ret3 (cons 'a (cons 'b (cons 'c ()))))
         (ret4 (let ((last (cons 'c ())))
                 (cons last (cons 'b last))))
         (ret7 (let* ((last (cons 'c ()))
                     (middle (cons last last)))
                 (cons middle middle)))
         (retno (let* ((last (cons 'c ()))
                      (lst (cons 'a (cons 'b last))))
                  (<change>
                     (set-cdr! last lst)
                     ())
                  lst)))
   (= 3 (count-pairs ret3) (count-pairs ret4) (count-pairs ret7) (count-pairs retno)))