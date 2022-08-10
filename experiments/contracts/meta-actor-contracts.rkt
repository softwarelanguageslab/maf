;; An implementation of actor contracts based on a simple meta-actor protocol

;; baseMirror is a mirror that programmers will probably never use since it is provided by the interpreter. 
(define baseMirror (behavior (base)
   (messages 
     ((send receiver message) 
      ;; send is implemented according to the mirror of this actor
      (meta/send receiver message))
     ((receive message) 
      ;; receive is implemented according to the mirror of this actor
      (meta/receive message))
     ((become new-behavior)
      ;; become is implemented according to the mirror of this actor
      (meta/become new-behavior)))))

;; mirror that supports handling ensure/c messages
(define ensure-mirror (behavior/extends baseMirror (base) 
   (messages 
     ((receive (contracted-message tag arguments ensures))
      (meta/receive (message tag arguments))
      (become ensure-mirror-with-ensures)))))

(define ensureMirrorStart (behavior "ensure-mirror-start"
