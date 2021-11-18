; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((inport #f)
         (outport #f)
         (catport (lambda (port)
                    (let ((x (read-char port)))
                       (if (eof-object? x)
                          (<change>
                             (close-output-port outport)
                             (begin
                                (catport port)
                                (write-char x outport)))
                          (<change>
                             (begin
                                (write-char x outport)
                                (catport port))
                             (close-output-port outport))))))
         (go (lambda ()
               (<change>
                  (set! inport (open-input-file "input.txt"))
                  ((lambda (x) x) (set! inport (open-input-file "input.txt"))))
               (set! outport (open-output-file "output.txt"))
               (catport inport)
               (close-input-port inport))))
   (go))