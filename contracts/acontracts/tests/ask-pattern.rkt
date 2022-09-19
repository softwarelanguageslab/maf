#lang racket

(require acontracts)
(require acontracts/interaction)

(define player (actor (team-id)
                  (get-team (sender) (reply sender team-id))))

(define player/c 
  (behavior/c (integer?)
    (ask/c get-team () integer?)))

(define player-actor (create/c player/c player))
(await (ask player get-team)
