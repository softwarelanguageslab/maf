;; Small program that tests whether the concrete interpreter rejects programs that sends references to futures as part of the messages
(define foo (actor "foo" ()))
(define foo-actor (create foo))
(define fut (wait-for-termination foo-actor))
(send foo-actor bar fut)
