; Changes:
; * removed: 3
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 3
(letrec ((inport #f)
         (nl #f)
         (nw #f)
         (nc #f)
         (inword #f)
         (wcport (lambda (port)
                   (let ((x (read-char port)))
                      (<change>
                         (if (eof-object? x)
                            (begin
                               (list nl nw nc))
                            (begin
                               (set! nc (+ nc 1))
                               (if (char=? x #\
) (set! nl (+ nl 1)) #f)
                               (if (let ((__or_res (char=? x #\ ))) (if __or_res __or_res (char=? x #\
)))
                                  (set! inword #f)
                                  (if (not inword)
                                     (begin
                                        (set! nw (+ nw 1))
                                        (set! inword #t))
                                     #f))
                               (wcport port)))
                         ((lambda (x) x)
                            (if (eof-object? x)
                               (begin
                                  (list nl nw nc))
                               (begin
                                  (<change>
                                     (set! nc (+ nc 1))
                                     ())
                                  (<change>
                                     (if (char=? x #\
) (set! nl (+ nl 1)) #f)
                                     ())
                                  (<change>
                                     (if (let ((__or_res (char=? x #\ ))) (if __or_res __or_res (char=? x #\
)))
                                        (set! inword #f)
                                        (if (not inword)
                                           (begin
                                              (set! nw (+ nw 1))
                                              (set! inword #t))
                                           #f))
                                     ((lambda (x) x)
                                        (if (let ((__or_res (char=? x #\ ))) (if __or_res __or_res (char=? x #\
)))
                                           (set! inword #f)
                                           (if (not inword)
                                              (<change>
                                                 (begin
                                                    (set! nw (+ nw 1))
                                                    (set! inword #t))
                                                 #f)
                                              (<change>
                                                 #f
                                                 (begin
                                                    #t
                                                    (set! inword #t)))))))
                                  (wcport port))))))))
         (go (lambda ()
               (set! inport (open-input-file "input.txt"))
               (set! nl 0)
               (set! nw 0)
               (<change>
                  (set! nc 0)
                  ((lambda (x) x) (set! nc 0)))
               (set! inword #f)
               (let ((result (wcport inport)))
                  (close-input-port inport)
                  result))))
   (equal? (go) (__toplevel_cons 31102 (__toplevel_cons 851820 (__toplevel_cons 4460056 ())))))