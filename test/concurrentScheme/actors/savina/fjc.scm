;; Adapted from Savina benchmarks ("Frok Join (actor creation)" benchmark, coming from JGF
(define N (int-top))
(define (perform-computation theta)
  (let ((sint (+ 1 theta)))
    (* sint sint)))
(define forkjoin-actor
  (actor "forkjoin" ()
           (message ()
                    (perform-computation 37.2)
                    (terminate))))
(define loop (lambda (n)
               (if (= n N)
                   'done
                   (begin
              (send (create forkjoin-actor) message)
              (loop (+ n 1))))))
(loop 0)
