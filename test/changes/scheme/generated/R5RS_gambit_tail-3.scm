; Changes:
; * removed: 1
; * added: 2
; * swaps: 0
; * negated predicates: 1
(letrec ((inport #f)
         (outport #f)
         (readline (lambda (port line-so-far)
                     (let ((x (read-char port)))
                        (if (<change> (eof-object? x) (not (eof-object? x)))
                           x
                           (if (char=? x #\
)
                              (list->string (reverse (cons x line-so-far)))
                              (readline port (cons x line-so-far)))))))
         (tail-r-aux (lambda (port file-so-far)
                       (let ((x (readline port ())))
                          (<change>
                             ()
                             display)
                          (if (eof-object? x)
                             (begin
                                (display file-so-far outport)
                                (<change>
                                   ()
                                   (close-output-port outport))
                                (close-output-port outport))
                             (tail-r-aux port (cons x file-so-far))))))
         (tail-r (lambda (port)
                   (tail-r-aux port ())))
         (go (lambda ()
               (set! inport (open-input-file "input.txt"))
               (set! outport (open-output-file "output.txt"))
               (<change>
                  (tail-r inport)
                  ())
               (close-input-port inport))))
   (go))