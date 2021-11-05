; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
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
               (<change>
                  (set! inport (open-input-file "input.txt"))
                  (set! outport (open-output-file "output.txt")))
               (<change>
                  (set! outport (open-output-file "output.txt"))
                  (set! inport (open-input-file "input.txt")))
               (catport inport)
               (<change>
                  (close-input-port inport)
                  ((lambda (x) x) (close-input-port inport))))))
   (go))