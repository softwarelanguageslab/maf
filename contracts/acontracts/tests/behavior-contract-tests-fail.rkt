#lang racket

(require acontracts)

(enable-contracts!)

(define player 
  (actor "player" (team-id)
    (get-team (reply-to)
              ; REMOVED: (reply reply-to team-id) -- should make ensures/c fail
              (become player team-id))))

(define player/c (behavior/c 
                   (integer?)
                   (get-team (actor?) (ensures/c ((reply (integer?)))))))

(define player-actor (create/c player/c player 42))

(define display-actor (create (actor "display-actor" () 
                                     (answer (v) (displayln v) (terminate)))))

(send player-actor get-team display-actor)
(wait-until-termination display-actor)
