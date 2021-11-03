; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((inport #f)
         (outport #f)
         (catport (lambda (port)
                    (<change>
                       ()
                       (display (write-char x outport)))
                    (let ((x (read-char port)))
                       (if (eof-object? x)
                          (close-output-port outport)
                          (begin
                             (write-char x outport)
                             (catport port))))))
         (go (lambda ()
               (set! inport (open-input-file "input.txt"))
               (set! outport (open-output-file "output.txt"))
               (catport inport)
               (close-input-port inport))))
   (go))