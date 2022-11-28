#lang racket

;;; This example adds protection to an actor A 
;;; through the an actor B that provides access control for A.
;;;
;;; Any message that targets actor A should therefore be processed
;;; by B first, which can decline the message, and should then be forwarded 
;;; to actor A. The issue is that actor A might leak a self-reference 
;;; when replying to the message.
;;;
;;; This vulnerability can be mitigated by only allowing sends from 
;;; actor A to be send through actor B, which sanitizes the payload, 
;;; or by checking whether the payload contains a self-reference 
;;; before allowing the send to happen in actor A.
;;;
;;; The latter option is implemented in this file.
;;;
;;; ----------        -----------       ------------
;;; | client |   -m-> | Proxy B |  -m-> | Target A |
;;; ----------        -----------       ------------
;;;
;;; @author Bram Vandenbogaerde
;;; @year 2022

(require acontracts/actors)

;; target actor A
(define A (actor "A" () 
            (good-message (r) 
               (reply r 'ok)
               (same-behavior))
            (bad-message (r)
               (reply r self)
              (same-behavior))))


;; mirror for security
(define (security-mirror base)
  (create (mirror "security" ()
      ;; create is not limited
      (create (interpreter mirror beh arguments)
         (reply interpreter (apply base/create mirror beh arguments))
         (same-behavior))
      ;; receive is not limited
      (receive (interpreter msg behavior sender sender-location)
               (reply interpreter (lookup-handler behavior (message-tag msg)))
               (same-behavior))
      ;; send IS limited
      (send (interpreter envelop sloc) 
            (let 
              ((payload (message-payload (envelope-message envelop))))

              (if (member base payload)
                  (base/fail interpreter "cannot send message containing a self reference")
                  (reply interpreter (send-envelope envelop)))
              (same-behavior))))))

;; proxy actor B
(define (B target)
  (let 
    ((b (actor "B" (target-actor)
            (good-message (r) 
               (send target-actor good-message r)
               (same-behavior))
            (bad-message (r)
               (send target-actor bad-message r)
               (same-behavior)))))

    (create b (create-with-mirror security-mirror target ))))

(define b (B A))
(await (ask b good-message))
(await (ask b bad-message))
(print-statistics)
