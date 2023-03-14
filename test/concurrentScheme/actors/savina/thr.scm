;; Adapted from Savina benchmarks ("Thread Ring" benchmark, coming from Theron)
(define N (int-top))
(define R (int-top))

(define (build-vector n f)
  (letrec ((v (make-vector n #f))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define threadring-actor
 (actor "thread-ring" (id actors-in-ring next-actor)
          (ping (pings-left)
                (if (> pings-left 0)
                    (begin
                      (send next-actor ping (- pings-left 1))
                      (become threadring-actor id actors-in-ring next-actor))
                    (begin
                      (send next-actor exit actors-in-ring)
                      (become threadring-actor id actors-in-ring next-actor))))
          (data (next)
                (become threadring-actor id actors-in-ring next))
          (exit (exits-left)
                (if (> exits-left 1) ; different from original benchmark (original will send an exit to a dead actor)
                    (send next-actor exit (- exits-left 1))
                    #f)
                (terminate))))

(define ring-actors (build-vector N
                           (lambda (i) (create threadring-actor i N #f))))
(define loop-next (lambda (i)
             (if (= i N)
                 'done
                 (begin
                   (send (vector-ref ring-actors i)
                           data (vector-ref ring-actors (modulo (+ i 1) N)))
                   (loop-next (+ i 1))))))
(loop-next 0)
(send (vector-ref ring-actors 0) ping R)
