#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Actor API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax base/send 
  (syntax-rules ()
    ((base/send receiver tag arguments ...)
     (actor-send actor (message tag (list arguments ...))))))

;; a regular message, with no contracts attached 
(struct message (tag arguments))

(struct actor (thread))

(define-syntax base/create 
  (syntax-rules ()
    ((create behavior arguments ...) 
     (actor (thread (lambda () 
         (let loop ((beh behavior))
           (let ((msg (thread-receive)))
            (loop (receive msg beh))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Private Actor API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (actor-send actor msg)
  (let ((thd (actor-thread actor)))
    (thread-send thd msg)))

(define (receive message behavior)
  (mirror-handle-receive behavior message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Message handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a message handler 
(struct message-handler (tag handler)
   #:property prop:procedure (lambda (msg msg-tag . args)
      (if (eq? (message-handler-tag msg) msg-tag)
          (apply (message-handler-handler msg) args)
          'invalid)))

(define-syntax messages 
  (syntax-rules ()
    ((messages ((tag (param ...) body ...) ...))
     (lambda (msg-tag . arguments)
       ;; try the message handlers in order, until one does not produce 'invalid
       (define (loop handler) 
         (if (null? handler)
           (error (format "no suitable handler for ~a found" msg-tag))
            (let ((res (apply (car handler) (cons msg-tag arguments))))
              (if (eq? res 'invalid)
                (loop (cdr handler))
                res))))
       (list (message-handler tag (lambda (param ...) body ...)) ...)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Behaviors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct behavior/s (constructor)
        #:property prop:procedure (struct-field-index constructor)
        #:reflection-name 'behavior)

(define-syntax (behavior stx)
  (syntax-case stx ()
    ((behavior name ((param param-contract) ...)
       handler)
     #'(behavior/s (lambda (param ...)
       ;; return a receival lambda
       (lambda (msg-tag . arguments)
         (apply handler (cons msg-tag arguments))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ask pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax ask
  (syntax-rules ()
    ((ask receiver tag param ...) 
     (let*
       ((res (mcons 'response '()))
        (reply-to (base/create (behavior "reply-to" () (messages ((reply (v) (set-mcdr! res v) (terminate))))))))

       (base/send receiver tag reply-to param ...)
       ;; representation of a "future", together with await they form a compatible API for what 
       ;; is implemented in MAF
       (lambda ()
          (actor-wait reply-to)
          (mcdr res))))))

(define (await fut)
  (fut))

(define-syntax reply 
  (syntax-rules ()
    ((reply to v) 
     (begin 
        (displayln (format "replying to ~a with ~a" to v))
        (send to reply v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mirrors 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; each actor has its own mirror
(define $mirror$ (make-parameter))

;; the structure that holds the actor that serves as a mirror for the current actor
(struct mirror (handler-actor))

;; a base mirror does not have any handling actor
(define base-mirror (mirror #f))

;; called by the base layer to install a mirror in the newly created actor
(define (install-mirror) 
  ($mirror$ base-mirror))

;; receives the current mirror of the actor
(define (current-mirror)
  ($mirror$))

;; handle a message receive, if there is an active mirror (that is not base) we will forward it to that mirror
(define (mirror-handle-receive behavior message) 
  (if (current-mirror)
      ;; if there is a mirror, send it a message, and stop processing
      (ask (current-mirror) receive message)
      ;; otherwise we apply the base layer semantics
      (apply behavior (message-tag message) (message-arguments message))))
