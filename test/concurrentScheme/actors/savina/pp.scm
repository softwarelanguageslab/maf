;; Adapted from Savina benchmark ("Ping Pong" benchmarks, coming from Scala)
(define ping-actor
  (actor "ping" (count pong)
           (start ()
                  (send pong send-ping a/self)
                  (become ping-actor (- count 1) pong))
           (ping ()
                 (send pong send-ping a/self)
                 (become ping-actor (- count 1) pong))
           (send-pong ()
                      (if (> count 0)
                          (begin
                            (send a/self ping)
                            (become ping-actor count pong))
                          (begin
                            (send pong stop)
                            (terminate))))))
(define pong-actor
  (actor "pong" (count)
           (stop () (terminate))
           (send-ping (to)
                      (send to send-pong)
                      (become  pong-actor (+ count 1)))))
(define pong (create pong-actor 0))
(define N (int-top))
(define ping (create ping-actor N pong))
(send ping start)
