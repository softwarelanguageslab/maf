;; An example program for testing future semantics in actor programs
;; assert(ret_main == 42)

(define player (actor "player" (team-id)
                      (get-team (reply-to) (reply reply-to team-id))))


(define player-actor (create player 42))
(define id (await (ask player-actor get-team)))
id 

