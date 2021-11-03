; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
(letrec ((inport #f)
         (outport #f)
         (catport (lambda (port)
                    (let ((x (read-char port)))
                       (if (eof-object? x)
                          (close-output-port outport)
                          (begin
                             (write-char x outport)
                             (catport port))))))
         (go (lambda ()
               (set! inport (open-input-file "input.txt"))
               (<change>
                  (set! outport (open-output-file "output.txt"))
                  (catport inport))
               (<change>
                  (catport inport)
                  (set! outport (open-output-file "output.txt")))
               (<change>
                  ()
                  "output.txt")
               (close-input-port inport))))
   (go))