
(define mirror (mirror "mirror" () 
   ;; block all sends
   (send (interpreter m) 
         (reply-to interpreter 'ok)
         (base/send-envelope m))

   ;; keep the original handler
   (receive (interpreter msg handler) (reply-to interpreter handler))))

(define my-behavior (actor "test" () 
   (do-send (n) 
      ; should be blocked
      (send self do-send (+ n 1)))))

(define my-actor (create-with-mirror (create mirror) my-behavior))
;(define my-actor ( my-behavior))
(send my-actor do-send 1)
