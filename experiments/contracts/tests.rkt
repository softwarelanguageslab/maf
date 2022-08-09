#lang racket

(require "./actors/base.rkt")

(mirror! base-mirror-actor)

(define times-ran 0)
(define player 
  (behavior "player" ((team-id any?))
   (messages 
      ((get-team (sender) 
                 (debug "sending reply to" sender)
                 (reply sender team-id)
                 (set! times-ran (+ times-ran 1))
                 (become player team-id))))))


(define player-actor (create player 10))
(debug "got player-actor" player-actor)
(displayln (await (ask player-actor get-team)))
(debug "times-ran" times-ran)
