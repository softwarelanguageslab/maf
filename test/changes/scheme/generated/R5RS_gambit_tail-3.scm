; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
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
                          (if (eof-object? x)
                             (begin
                                (display file-so-far outport)
                                (close-output-port outport))
                             (tail-r-aux port (cons x file-so-far))))))
         (tail-r (lambda (port)
                   (tail-r-aux port ())))
         (go (lambda ()
               (<change>
                  (set! inport (open-input-file "input.txt"))
                  (set! outport (open-output-file "output.txt")))
               (<change>
                  (set! outport (open-output-file "output.txt"))
                  (set! inport (open-input-file "input.txt")))
               (tail-r inport)
               (<change>
                  (close-input-port inport)
                  ((lambda (x) x) (close-input-port inport))))))
   (go))