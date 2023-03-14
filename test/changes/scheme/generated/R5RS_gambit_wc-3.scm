; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((inport #f)
         (nl #f)
         (nw #f)
         (nc #f)
         (inword #f)
         (wcport (lambda (port)
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
                                  (<change>
                                     (list nl nw nc)
                                     ((lambda (x) x) (list nl nw nc))))
                               (begin
                                  (set! nc (+ nc 1))
                                  (<change>
                                     ()
                                     __or_res)
                                  (if (char=? x #\
) (set! nl (+ nl 1)) #f)
                                  (if (let ((__or_res (char=? x #\ ))) (if (<change> __or_res (not __or_res)) __or_res (char=? x #\
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
               (set! nc 0)
               (<change>
                  (set! inword #f)
                  (let ((result (wcport inport)))
                     (close-input-port inport)
                     result))
               (<change>
                  (let ((result (wcport inport)))
                     (close-input-port inport)
                     result)
                  (set! inword #f)))))
   (equal? (go) (__toplevel_cons 31102 (__toplevel_cons 851820 (__toplevel_cons 4460056 ())))))