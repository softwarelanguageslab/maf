; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((inport #f)
         (outport #f)
         (catport (lambda (port)
                    (let ((x (read-char port)))
                       (<change>
                          (if (eof-object? x)
                             (close-output-port outport)
                             (begin
                                (write-char x outport)
                                (catport port)))
                          ((lambda (x) x)
                             (if (eof-object? x)
                                (close-output-port outport)
                                (begin
                                   (write-char x outport)
                                   (catport port))))))))
         (go (lambda ()
               (<change>
                  (set! inport (open-input-file "input.txt"))
                  ())
               (set! outport (open-output-file "output.txt"))
               (catport inport)
               (close-input-port inport))))
   (go))