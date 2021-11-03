; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 1
(letrec ((inport #f)
         (outport #f)
         (catport (lambda (port)
                    (let ((x (read-char port)))
                       (if (<change> (eof-object? x) (not (eof-object? x)))
                          (close-output-port outport)
                          (begin
                             (<change>
                                (write-char x outport)
                                (catport port))
                             (<change>
                                (catport port)
                                (write-char x outport)))))))
         (go (lambda ()
               (set! inport (open-input-file "input.txt"))
               (set! outport (open-output-file "output.txt"))
               (catport inport)
               (close-input-port inport))))
   (go))