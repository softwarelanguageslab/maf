(define NumSeries (int-top))
(define NumComputers NumSeries)
(define NumWorkers NumSeries)
(define StartRate (int-top))
(define Increment (int-top))

(define (build-vector1 n f)
  (letrec ((v (make-vector n (f 0)))
           (loop1 (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop1 (+ i 1)))
                       v))))
    (loop1 1)))

(define (build-vector2 n f)
  (letrec ((v (make-vector n (f 0)))
           (loop2 (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop2 (+ i 1)))
                       v))))
    (loop2 1)))

(define (vector-foreach f v)
  (letrec ((loop (lambda (i)
                   (if (< i (vector-length v))
                       (begin
                         (f (vector-ref v i))
                         (loop (+ i 1)))
                       'done))))
    (loop 0)))

(define master-init
  (actor "master-init" ()
           (start ()
                  (let* ((computers
                          (build-vector1 NumComputers
                                        (lambda (i)
                                          (let* ((rate (+ StartRate (* i Increment))))
                                            (create rate-computer rate)))))
                         (workers
                          (build-vector2 NumWorkers
                                        (lambda (i)
                                          (let* ((rate-computer
                                                 (vector-ref computers (modulo i NumComputers)))
                                                 (start-term (* i Increment))
                                                 (actorRef (create series-worker a/self rate-computer start-term)))
                                            (send actorRef next-term)
                                            (send actorRef get-term)
                                            actorRef
                                            )))))
                    (become master computers workers NumWorkers 0 0)))))
(define master
  (actor "master" (computers workers num-work-requested num-work-received terms-sum)
           (result (term)
                   (if (= (+ num-work-received 1) num-work-requested)
                       (begin
                         (vector-foreach (lambda (a) (send a stop)) computers)
                         (vector-foreach (lambda (a) (send a stop)) workers)
                         (terminate))
                       (become master computers workers num-work-requested (+ num-work-received 1) (+ terms-sum term))))))

(define series-worker-wait
  (actor "series-worker-wait" (master computer)
         (next-term ()
                    (send a/self next-term)
                    (become series-worker-wait master computer))
         (result (term)
                 (become series-worker master computer term))
         (get-term ()
                   (send a/self get-term)
                   (become series-worker-wait master computer))
         (stop ()
               (send a/self stop)
               (become series-worker-wait master computer))))
(define series-worker
  (actor "series-worker" (master computer cur-term)
           (next-term ()
                      (send computer compute cur-term a/self)
                      ;; (become series-worker master computer cur-term)
                      (become series-worker-wait master computer)
                      )
           (result (term)
                   (become series-worker master computer term))
           (get-term ()
                     (send master result cur-term)
                     (become series-worker master computer cur-term))
           (stop () (terminate))))

(define (compute-next-term cur rate)
  (* rate cur (- 1 cur)))

(define rate-computer
  (actor "rate-computer" (rate)
           (compute (term sender)
                    (send sender result (compute-next-term term rate))
                    (become rate-computer rate))
           (stop ()
                 (terminate))))

;; (define master-actor (create master #f #f 0 0 0))
(define master-actor (create master-init))
(send master-actor start)
