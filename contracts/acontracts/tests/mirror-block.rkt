#lang racket

(require "../actors.rkt")

(define mymirror (mirror "mirror" () 
   ;; block all sends
   (send (interpreter m) 
         (reply-to interpreter 'fail)
         (error "could not send"))
   ;; keep the original handler
   (receive (interpreter msg handler) 
            (reply-to interpreter handler)
            (become mymirror))))

(define my-behavior (actor "test" () 
   (do-send (n) 
      ; should be blocked
      (send self do-send (+ n 1)))))

(define my-actor (create-with-mirror (create mymirror) my-behavior))
;(define my-actor ( my-behavior))
(send my-actor do-send 1)
(wait-until-termination my-actor)
