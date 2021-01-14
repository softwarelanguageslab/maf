;;; CAT -- One of the Kernighan and Van Wyk benchmarks.

(define inport #f)
(define outport #f)

(define (catport port)
  (let ((x (read-char port)))
    (if (eof-object? x)
        (close-output-port outport)
        (begin
          (write-char x outport)
          (catport port)))))

(define (go)
  (set! inport (open-input-file "input.txt"))
  (set! outport (open-output-file "output.txt"))
  (catport inport)
  (close-input-port inport))

(go)
