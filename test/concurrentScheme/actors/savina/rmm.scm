(define NumWorkers (int-top))
(define DataLength (int-top))
(define NumBlocks (* DataLength DataLength))
(define BlockThreshold (int-top))

(define (build-vector n f)
  (letrec ((v (make-vector n #f))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))
(define (vector-foreach f v)
  (letrec ((loop (lambda (i)
                   (if (< i (vector-length v))
                       (begin
                         (f (vector-ref v i))
                         (loop (+ i 1)))
                       'done))))
    (loop 0)))

(define master-actor
  (actor "master-actor" (workers num-workers-terminated num-work-sent num-work-completed)
           (work (srA scA srB scB srC scC num-blocks dim priority)
                 (let ((index (modulo (inexact->exact (floor (+ srC scC))) NumWorkers)))
                   (send (vector-ref workers index) work srA scA srB scB srC scC num-blocks dim priority)
                   (become master-actor workers num-workers-terminated (+ num-work-sent 1) num-work-completed)))
           (done ()
                 (if (= (+ num-work-completed 1) num-work-sent)
                     (vector-foreach (lambda (a) (send a stop)) workers)
                     #t)
                 (become master-actor workers num-workers-terminated num-work-sent (+ num-work-completed 1)))
           (stop ()
                 (if (= (+ num-workers-terminated 1) NumWorkers)
                     (begin
                       (send result stop)
                       (terminate))
                     (become master-actor workers (+ num-workers-terminated 1) num-work-sent num-work-completed)))))

(define master-actor-init
  (actor "master-actor" ()
           (start ()
                  (become master-actor
                            (build-vector NumWorkers (lambda (i)
                                                       (let* ((w (create worker-actor a/self i)))
                                                         (if (= i 0)
                                                             (send w work 0 0 0 0 0 0 NumBlocks DataLength 0)
                                                             #t)
                                                         w)))
                            0 0 0))))

(define (A i j) i)
(define (B i j) j)

(define result-actor
   (actor "result-actor" (C)
            (add (i j v)
                 (vector-set! (vector-ref C j) i (+ (vector-ref (vector-ref C j) i) (inexact->exact (floor v))))
                 (become result-actor C))
            (stop () (terminate))))
(define result
  (create result-actor
   (build-vector DataLength (lambda (i) (make-vector DataLength 0)))))

(define worker-actor
  (actor "worker-actor" (master id)
           (work (srA scA srB scB srC scC num-blocks dim priority)
                 (if (> num-blocks BlockThreshold)
                     (let* ((new-dim (inexact->exact (floor (/ dim 2))))
                            (new-num-blocks (inexact->exact (floor (/ num-blocks 4))))
                            (new-priority (+ priority 1)))
                       (send master work
                               srA scA srB scB srC scC new-num-blocks new-dim new-priority)
                       (send master work
                               srA (+ scA new-dim) (+ srB new-dim) scB srC scC new-num-blocks new-dim new-priority)
                       (send master work
                               srA scA srB (+ scB new-dim) srC (+ scC new-dim) new-num-blocks new-dim new-priority)
                       (send master work
                               srA (+ scA new-dim) (+ srB new-dim) (+ scB new-dim) srC (+ scC new-dim) new-num-blocks new-dim new-priority)
                       (send master work
                               (+ srA new-dim) scA srB scB (+ srC new-dim) scC new-num-blocks new-dim new-priority)
                       (send master work
                               (+ srA new-dim) (+ scA new-dim) (+ srB new-dim) scB (+ srC new-dim) scC new-num-blocks new-dim new-priority)
                       (send master work
                               (+ srA new-dim) scA srB (+ scB new-dim) (+ srC new-dim) (+ scC new-dim) new-num-blocks new-dim new-priority)
                       (send master work
                               (+ srA new-dim) (+ scA new-dim) (+ srB new-dim) (+ scB new-dim) (+ srC new-dim) (+ scC new-dim) new-num-blocks new-dim new-priority))
                     (letrec ((endR (+ srC dim))
                              (endC (+ scC dim))
                              (loopi (lambda (i)
                                      (if (= i endR)
                                          #t
                                          (letrec ((loopj (lambda (j)
                                                            (if (= j endC)
                                                                (loopi (+ i 1))
                                                                (letrec ((loopk (lambda (k)
                                                                                  (if (= k dim)
                                                                                      (loopj (+ j 1))
                                                                                      (let ((r (+ (A i (+ scA k)) (B (+ srB k) j))))
                                                                                        (send result add i j r)
                                                                                        (loopk (+ k 1)))))))
                                                                  (loopk 0))))))
                                            (loopj scC))))))
                       (loopi srC)))
                 (send master done)
                 (become worker-actor master id))
           (stop ()
                 (send master stop)
                 (terminate))))
(define master (create master-actor-init))
(send master start)
