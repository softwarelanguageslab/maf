#lang racket

;; runs the Alexander macro expander (see macro-expander.rkt)
;; on the input file given in the first argument and outputs 
;; to the file given in the first argument

(require "./macro-expander.rkt")

;; Use Racket's `read` function as long as the input port keeps 
;; producing input 
(define (read-until-eof input)
  (let ((in (read input)))
    (if (eof-object? in)
        '()
        (cons in (read-until-eof input)))))

(define argv (current-command-line-arguments))

(unless (>= (vector-length argv) 2)
  (error "not enough arguments supplied to this program"))

(define input-name (vector-ref argv 0))
(define output-name (vector-ref argv 1))

(displayln (format "Input file: ~s" input-name))
(displayln (format "Output file: ~s" output-name))

(define input-file (open-input-file input-name))
(define output-file (open-output-file output-name))

(define input-program (read-until-eof input-file))
(define output (expand-program input-program))
(pretty-write output output-file)

(close-input-port input-file)
(close-output-port output-file)
