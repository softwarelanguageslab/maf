; Changes:
; * removed: 1
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((inport #f)
         (nl #f)
         (nw #f)
         (nc #f)
         (inword #f)
         (wcport (lambda (port)
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
                                     (<change>
                                        (set! nw (+ nw 1))
                                        ())
                                     (<change>
                                        ()
                                        (set! inword #t))
                                     (set! inword #t))
                                  #f))
                            (<change>
                               ()
                               (not inword))
                            (wcport port))))))
         (go (lambda ()
               (set! inport (open-input-file "input.txt"))
               (set! nl 0)
               (set! nw 0)
               (set! nc 0)
               (<change>
                  (set! inword #f)
                  ((lambda (x) x) (set! inword #f)))
               (let ((result (wcport inport)))
                  (close-input-port inport)
                  result))))
   (equal? (go) (__toplevel_cons 31102 (__toplevel_cons 851820 (__toplevel_cons 4460056 ())))))