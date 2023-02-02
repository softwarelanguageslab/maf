#lang racket

(require acontracts)
(parse-cmdline!)

;; Adapted from Savina benchmarks ("Counting Actor" benchmarks, coming from Theron)
(define N 100)
(define producer-actor
  (actor "producer" (counter)
           (increment ()
                      (letrec ((loop (lambda (n)
                                       (if (> n 0)
                                           (begin
                                             (send counter increment)
                                             (loop (- n 1)))
                                           'done))))
                        (loop N)
                        (send counter retrieve self)
                        (become producer-actor counter)))
           (result (count)
                   (if (= count N)
                       (display "Success!")
                       (error "Error!"))
                   (terminate))))
(define counting-actor
 (actor "counting" (count)
          (increment ()
                     (become counting-actor (+ count 1)))
          (retrieve (to)
                    (send to result count)
                    (terminate))))

(define counter/c 
  (behavior/c (integer?)
    (increment () unconstrained/c)
    (retrieve (actor?) (lambda (payload) (ensures/c (result (integer?) unconstrained/c (specific-recipient (car payload))))))))

(define producer/c 
  (behavior/c (integer?)
    (increment () unconstrained/c)
    (result (integer?) unconstrained/c)))

(define counter (create/c counter/c counting-actor 0))
(define producer (create/c producer/c producer-actor counter))
(send producer increment)

(print-statistics)
