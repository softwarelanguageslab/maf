#lang racket

(require acontracts) 

;; From Agha, 1986, p. 54
;; Adapted with contracts by Bram Vandenbogaerde
(letrec ((fact-actor
          (actor "fac" ()
                 (compute (n customer)
                          (if (= n 0)
                              (send customer result 1)
                              (let ((c (create customer-actor n customer)))
                                (send self compute (- n 1) c)))
                          (become fact-actor))))
         (customer/c (lambda () (behavior/c (integer? (customer/c))
                                            (ensure-sends-only/c (result (integer?))))))
         (fact-actor/c (behavior/c () 
                           (compute (integer? (customer/c))
                                    (ensure-sends-one/c (result (integer?))
                                                        (compute (integer? customer/c))))))
         (customer-actor
          (actor "customer" (n customer)
                 (result (k)
                         (send customer result (* n k))
                         (become customer-actor n customer))))
         (display-actor
          (actor "display" ()
                 (result (n) (display n))))
         (f (create/c fact-actor/c fact-actor))
         (disp (create/c custumer-actor display-actor)))
  (send f compute 5 disp))

(print-statistics)
