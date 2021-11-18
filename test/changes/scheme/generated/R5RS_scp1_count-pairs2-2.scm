; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((count-pairs (lambda (lst)
                        (let ((path ()))
                           (letrec ((count (lambda (current)
                                             (<change>
                                                ()
                                                (set! path (cons current path)))
                                             (if (null? current)
                                                0
                                                (if (not (pair? current))
                                                   0
                                                   (if (memq current path)
                                                      0
                                                      (begin
                                                         (set! path (cons current path))
                                                         (<change>
                                                            (+ 1 (count (car current)) (count (cdr current)))
                                                            ((lambda (x) x) (+ 1 (count (car current)) (count (cdr current))))))))))))
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