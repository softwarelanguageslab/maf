(define NumRounds (int-top))
(define NumSmokers (int-top))

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

(define (notify-random-smoker smoker-actors)
  (send (vector-ref smoker-actors (random NumSmokers)) start-smoking (+ (random 1000) 10)))
(define arbitrator
  (actor "arbitrator" (smoker-actors rounds-so-far)
           (create-smokers ()
                           (become arbitrator (build-vector NumSmokers (lambda (i) (create smoker a/self))) 0))
           (start ()
                  (notify-random-smoker smoker-actors)
                  (become arbitrator smoker-actors rounds-so-far))
           (started-smoking ()
                            (if (= (+ rounds-so-far 1) NumRounds)
                                (begin
                                  (vector-foreach (lambda (s) (send s exit)) smoker-actors)
                                  (terminate))
                                (begin
                                  (notify-random-smoker smoker-actors)
                                  (become arbitrator smoker-actors (+ rounds-so-far 1)))))))

(define smoker
  (actor "smoker" (arbitrator)
           (start-smoking (wait)
                          (send arbitrator started-smoking)
                          (become smoker arbitrator))
           (exit () (terminate))))

(define arbitrator-actor (create arbitrator #f 0))
(send arbitrator-actor create-smokers)
(send arbitrator-actor start)
