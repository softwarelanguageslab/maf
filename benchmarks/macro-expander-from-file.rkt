#lang racket

;; runs the Alexander macro expander (see macro-expander.rkt)
;; on the input file given in the first argument and outputs 
;; to the file given in the first argument

(require "./macro-expander.rkt")

(define argv (current-command-line-arguments))

(unless (>= (vector-length argv) 3)
  (error "not enough arguments supplied to this program"))

(define input-name (vector-ref argv 1))
(define output-name (vector-ref argv 2))

(displayln (format "Input file: ~s" input-name))
(displayln (format "Output file: ~s" output-name))

(define input-file (open-input-file input-name))
(define output-file (open-output-file output-name))

(define output (expand-program (read input-file)))
(write output output-file)

(close-input-port input-file)
(close-output-port output-file)
