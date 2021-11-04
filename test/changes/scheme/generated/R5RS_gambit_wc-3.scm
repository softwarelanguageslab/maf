; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((inport #f)
         (nl #f)
         (nw #f)
         (nc #f)
         (inword #f)
         (wcport (lambda (port)
                   (<change>
                      ()
                      port)
                   (<change>
                      (let ((x (read-char port)))
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
                               (wcport port))))
                      ((lambda (x) x)
                         (let ((x (read-char port)))
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
                                  (wcport port))))))))
         (go (lambda ()
               (set! inport (open-input-file "input.txt"))
               (set! nl 0)
               (set! nw 0)
               (<change>
                  ()
                  (display inport))
               (set! nc 0)
               (set! inword #f)
               (let ((result (wcport inport)))
                  (close-input-port inport)
                  (<change>
                     ()
                     (display close-input-port))
                  result))))
   (<change>
      (equal? (go) (__toplevel_cons 31102 (__toplevel_cons 851820 (__toplevel_cons 4460056 ()))))
      ((lambda (x) x)
         (equal? (go) (__toplevel_cons 31102 (__toplevel_cons 851820 (__toplevel_cons 4460056 ())))))))