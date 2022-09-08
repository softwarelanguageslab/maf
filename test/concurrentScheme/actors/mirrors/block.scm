(define mirror (mirror "mirror" () 
   ;; block all sends
   (send (sender m) 
         (reply sender 'fail)
         (error "could not send"))

   ;; keep the original handler
   (receive (sender msg handler) (reply sender handler))))

(define my-behavior (actor "test" () 
   (do-send (n) 
      ; should be blocked
      (send self do-send (+ n 1)))))

(define my-actor (create-with-mirror (create mirror) my-behavior))
;(define my-actor ( my-behavior))
(send my-actor do-send 1)
