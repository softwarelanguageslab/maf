; Changes:
; * removed: 1
; * added: 2
; * swaps: 1
; * negated predicates: 0
(letrec ((inport #f)
         (outport #f)
         (readline (lambda (port line-so-far)
                     (<change>
                        ()
                        x)
                     (let ((x (read-char port)))
                        (if (eof-object? x)
                           x
                           (if (char=? x #\
)
                              (list->string (reverse (cons x line-so-far)))
                              (readline port (cons x line-so-far)))))))
         (tail-r-aux (lambda (port file-so-far)
                       (let ((x (readline port ())))
                          (if (eof-object? x)
                             (begin
                                (<change>
                                   (display file-so-far outport)
                                   (close-output-port outport))
                                (<change>
                                   (close-output-port outport)
                                   (display file-so-far outport)))
                             (tail-r-aux port (cons x file-so-far))))))
         (tail-r (lambda (port)
                   (<change>
                      ()
                      (tail-r-aux port ()))
                   (tail-r-aux port ())))
         (go (lambda ()
               (set! inport (open-input-file "input.txt"))
               (<change>
                  (set! outport (open-output-file "output.txt"))
                  ())
               (tail-r inport)
               (close-input-port inport))))
   (go))