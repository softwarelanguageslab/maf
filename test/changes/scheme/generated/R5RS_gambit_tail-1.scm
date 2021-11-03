; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 1
(letrec ((inport #f)
         (outport #f)
         (readline (lambda (port line-so-far)
                     (let ((x (read-char port)))
                        (if (eof-object? x)
                           x
                           (if (char=? x #\
)
                              (list->string (reverse (cons x line-so-far)))
                              (readline port (cons x line-so-far)))))))
         (tail-r-aux (lambda (port file-so-far)
                       (let ((x (readline port ())))
                          (if (<change> (eof-object? x) (not (eof-object? x)))
                             (begin
                                (display file-so-far outport)
                                (close-output-port outport))
                             (tail-r-aux port (cons x file-so-far))))))
         (tail-r (lambda (port)
                   (tail-r-aux port ())))
         (go (lambda ()
               (set! inport (open-input-file "input.txt"))
               (set! outport (open-output-file "output.txt"))
               (<change>
                  (tail-r inport)
                  (close-input-port inport))
               (<change>
                  (close-input-port inport)
                  (tail-r inport)))))
   (<change>
      ()
      go)
   (go))