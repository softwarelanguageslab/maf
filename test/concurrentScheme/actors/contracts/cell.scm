#lang racket

(parse-cmdline!)
;; If the analysis of the program is imprecise it will say that 
;; the error is reachable. However at run-time the error is unreachable 
;; since the cell contents will be 2 making the display-actor terminate.
(require acontracts)

(define cell
    (actor "cell" (content)
        (put (newcontent) 
             (become cell newcontent))
        (get (act) (send act value content) (become cell content))))

(define display-actor 
    (actor "display" ()
        (value (x) (if (= x 2) (terminate) (error "Error!")))))

(define display/c 
  (behavior/c () 
     (value (any/c) unconstrained/c)))

(define cell/c 
  (behavior/c (any/c)
    (put (any/c) unconstrained/c)
    (get (display/c) (lambda (payload) 
                       (ensures/c 
                          (value (any/c) unconstrained/c (specific-recipient (car payload))))))))


(define disp (create/c display/c display-actor))
(define c1 (create/c cell/c cell 1))
(define c2 (create/c cell/c cell 2))

(send c1 put 2)
(send c2 put 5)
(send c1 get disp)

(print-statistics)
