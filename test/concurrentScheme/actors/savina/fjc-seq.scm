;; Adapted from Savina benchmarks ("Frok Join (actor creation)" benchmark, coming from JGF
(letrec ((N 3)
         (perform-computation (lambda (theta)
                                (let ((sint (+ 1 theta))) ; was (sint (sin theta)) in the original implementation
                                  (* sint sint))))
         (forkjoin-actor
          (actor "forkjoin" ()
                 (message ()
                          (perform-computation 37.2)
                          (terminate)))))
  (send (create forkjoin-actor) message)
  (send (create forkjoin-actor) message)
  (send (create forkjoin-actor) message))
