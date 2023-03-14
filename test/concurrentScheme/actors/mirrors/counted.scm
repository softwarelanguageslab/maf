;; Example of actor that is only allowed to N messages. 

(define N 5)
(define counted-mirror (mirror "mirror" (n)
                       (send (interpreter m) 
                             (if (= n 0)
                               (begin (reply-to interpreter 'fail)
                                      (error "could not send message"))
                               (begin (reply-to interpreter 'ok)
                                      (base/send-envelope m)
                                      (become counted-mirror (- n 1)))))
                       (receive (interpreter msg handler) 
                                (reply-to interpreter handler)
                                (become counted-mirror n))))

(define my-behavior (actor "test" () 
                           (do-send (n) 
                                    ; should be blocked after five times
                                    (send self do-send (+ n 1)))))

(define my-actor (create-with-mirror (create counted-mirror N) my-behavior))
(send my-actor do-send 1)

