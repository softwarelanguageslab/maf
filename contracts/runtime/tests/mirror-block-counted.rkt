#lang racket

(require "../actors.rkt")
;; Example of actor that is only allowed to N messages. 

(define N 5)
(define counted-mirror (mirror "mirror" (n)
                       (send (interpreter m) 
                             (if (= n 0)
                               (begin 
                                 (base/fail interpreter "could not send message"))
                               (begin (reply-to interpreter 'ok)
                                      (displayln "continuing")
                                      (base/send-envelope m)
                                      (become counted-mirror (- n 1)))))
                       (receive (interpreter msg handler) 
                                (reply-to interpreter handler)
                                (become counted-mirror n))))

(define my-behavior (actor "test" () 
                           (do-send (n) 
                                    ; should be blocked after five times
                                    (send self do-send (+ n 1))
                                    (become my-behavior))))

(define my-actor (create-with-mirror (create counted-mirror N) my-behavior))
(send my-actor do-send 1)
(wait-until-termination my-actor)
