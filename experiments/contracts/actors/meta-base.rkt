#lang racket

(provide (except-out (all-defined-out) base-mirror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Actor API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax base/send 
  (syntax-rules ()
    ((base/send receiver tag arguments ...)
     (begin
        (actor-send receiver (message (quote tag) (list arguments ...)))))))

(define-syntax send 
  (syntax-rules ()
    ((send recv tag arguments ...)
     (meta-send recv (quote tag) (list arguments ...)))))

;; a regular message, with no contracts attached 
(struct message (tag arguments) #:transparent)

(struct actor (thread line column source) #:transparent)

(define (create/f init-behavior arguments line column source . mirror)
  (let ((behavior (apply init-behavior arguments)))
     (actor (thread (lambda () 
                      (if (null? mirror)
                          (install-mirror)
                          (mirror! (car mirror)))
                      (let loop ((beh behavior))
                        (let ((msg (thread-receive)))
                          (loop (receive msg beh))))))
            line column source)))


(define-syntax (base/create stx)
  (syntax-case stx ()
    ((create behavior arguments ...) 
     (let ((line (syntax-line stx))
           (column (syntax-column stx))
           (source (syntax-source stx)))
     #`(create/f behavior (list arguments ...) #,line #,column #,source #f)))))

(define (become new-behavior . args)
  (apply new-behavior args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Private Actor API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (debug . args)
   (for ([arg args]) (display arg) (display " "))
   (newline))

(define (actor-send actor msg)
  (let ((thd (actor-thread actor)))
    (thread-send thd msg)))

(define (receive message behavior)
  (mirror-handle-receive behavior message))

(define (terminate) 
  (kill-thread (current-thread)))

(define (actor-wait actor)
  (thread-wait (actor-thread actor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Message handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a message handler 
(struct message-handler (tag handler)
   #:property prop:procedure (lambda (msg msg-tag . args)
      (if (eq? (message-handler-tag msg) msg-tag)
          (begin 
            (debug "selected handler")
            (apply (message-handler-handler msg) args))
          'invalid)))

(define-syntax messages/extend
  (syntax-rules ()
     ((messages/extend base ((tag (param ...) body ...) ...))

      (let ((handler (messages ((tag (param ...) body ...) ...)))
            (next-handler base))
        (lambda (msg-tag . arguments)
          (debug "messages/extend: handling message" msg-tag arguments)
          (let ((result (apply handler msg-tag arguments)))
            (debug "got result for" msg-tag result)
            (if (eq? 'invalid result)
              (begin
                (debug "trying next handler")
                 (next-handler (message msg-tag arguments)))
              result)))))))

(define-syntax messages 
  (syntax-rules ()
    ((messages ((tag (param ...) body ...) ...))
     (lambda (msg-tag . arguments)
       ;; try the message handlers in order, until one does not produce 'invalid
       (define (loop handler) 
         (if (null? handler)
           'invalid
            (let ((res (apply (car handler) (cons msg-tag arguments))))
              (if (eq? res 'invalid)
                (loop (cdr handler))
                res))))
       (loop (list (message-handler (quote tag) (lambda (param ...) body ...)) ...))))))

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
       (lambda (msg)
         (let ((result (apply handler (message-tag msg) (message-arguments msg))))
           (if (eq? result 'invalid)
               (error (format "no handler for ~a found" msg))
               result))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ask pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax ask
  (syntax-rules ()
    ((ask receiver tag param ...) 
     (let*
       ((res (mcons 'response '()))
        (reply-to (base/create (behavior "reply-to" () (messages ((reply (v) #;(debug "received reply" v (quote tag)) (set-mcdr! res v) (terminate))))))))

       (debug "sending to" receiver (quote tag) reply-to (list param ...))
       (send receiver tag reply-to param ...)
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
     (let ((to$ to)
           (v$ v))
        (displayln (format "replying to ~a with ~a" to$ v$))
        (base/send to$ reply v$)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mirrors 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the structure that holds the actor that serves as a mirror for the current actor
(struct mirror (handler-actor))

;; the default mirror used by this application
;; changing this value results in a different mirror being used when creating a new actor
(define default-mirror (lambda ags #f))

;; each actor has its own mirror
(define $mirror$ (make-parameter (default-mirror #f)))

(define (mirror! new-mirror)
  ($mirror$ new-mirror))

;; Keeps track of the current meta-level 
;; for example if a send is doing a send then the meta-level increases.
(define $meta-level$ (make-parameter 0))

;; a base mirror does not have any handling actor
(define base-mirror (mirror #f))

;; called by the base layer to install a mirror in the newly created actor
(define (install-mirror) 
  ($meta-level$ 0)
  ($mirror$ (default-mirror ($mirror$))))

(define (default-mirror! new-mirror)
  (set! default-mirror new-mirror))

;; receives the current mirror of the actor
(define (current-mirror)
  ($mirror$))

(define-syntax with-increased-meta 
  (syntax-rules ()
    ((with-increased-meta exp)
     (begin 
       ($meta-level$ (+ 1 (current-meta)))
       (let ((res exp))
          ($meta-level$ (- 1 (current-meta)))
          res)))))

(define (current-meta)
  ($meta-level$))

;; handle a message receive, if there is an active mirror (that is not base) we will forward it to that mirror
(define (mirror-handle-receive behavior message) 
  ;(debug "meta-receiver:" behavior message)
  (if (and (current-mirror) (= (current-meta) 0))
      ;; if there is a mirror, send it a message, and stop processing
      (begin 
        (with-increased-meta 
          ((await (ask (current-mirror) receive behavior message)))))
      ;; otherwise we apply the base layer semantics
      (behavior message)))

(define (meta-send receiver tag arguments)
  (debug "mirror:" receiver tag arguments (current-mirror) (current-meta))
  (if (and (current-mirror) (= (current-meta) 0))
      (begin
         (with-increased-meta 
            (await (ask (current-mirror) send receiver tag arguments))))
      (actor-send receiver (message tag arguments))))
