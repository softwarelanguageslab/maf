#lang racket 
(parse-cmdline!)

(require acontracts)

(define stack-node 
  (actor "stack-node" (content link)
         (pop (customer)
              (if link
                (begin
                  (send customer message content)
                  (link))
                (begin
                  (error "popping an empty stack")
                  (terminate))))
         (push (v)
               (become stack-node v (lambda () (become stack-node content link))))))

(define display/c 
  (behavior/c () 
     (message (any/c) unconstrained/c)))

(define stack/c 
  (behavior/c  (any/c)
    (pop (display/c) (lambda (payload) 
                      (ensures/c (message (any/c) unconstrained/c (specific-recipient (car payload))))))
    (push (any/c) unconstrained/c)))

(define display-actor 
  (actor "display" ()
         (message (v) (display v) (become display-actor))))

(define disp (create/c display/c display-actor))
(define act (create/c stack/c stack-node #f #f))

(send act push (random 42))
(send act push #t)
(send act push 3)
(send act pop disp)

(print-statistics)
