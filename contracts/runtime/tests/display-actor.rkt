#lang racket 

(require "../actors.rkt")

(define displayer (actor "display" () 
                           (display (v) 
                                    (displayln v)
                                    (terminate))))

(define display-actor (create displayer))
(send display-actor display "hello world")
(wait-until-termination display-actor)

