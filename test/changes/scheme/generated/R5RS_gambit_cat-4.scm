; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((inport #f)
         (outport #f)
         (catport (lambda (port)
                    (<change>
                       ()
                       outport)
                    (let ((x (read-char port)))
                       (if (eof-object? x)
                          (close-output-port outport)
                          (begin
                             (write-char x outport)
                             (catport port))))))
         (go (lambda ()
               (<change>
                  (set! inport (open-input-file "input.txt"))
                  (set! outport (open-output-file "output.txt")))
               (<change>
                  (set! outport (open-output-file "output.txt"))
                  (set! inport (open-input-file "input.txt")))
               (<change>
                  (catport inport)
                  ((lambda (x) x) (catport inport)))
               (close-input-port inport))))
   (go))