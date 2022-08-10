#lang racket

(require (prefix-in base: "./meta-base.rkt"))
(provide (all-defined-out))


(define (meta-create behavior arguments)
  (if (base:current-mirror)
    (base:with-increased-meta
       (base:await (base:ask (base:current-mirror) create behavior arguments)))
    (base:create/f behavior arguments)))

(define base-mirror 
  (base:behavior "base-mirror" () 
   (base:messages 
     ((send (sender receiver tag arguments)
      (base:debug "meta/send:" sender receiver tag arguments)
      (base:reply sender (base:actor-send receiver (base:message tag arguments)))
      (base:become base-mirror))
     (create (sender behavior arguments)
      (base:debug "meta/create" behavior arguments)
      (base:reply sender (base:create/f behavior arguments))
      (base:become base-mirror))
     (receive (sender behavior message)
      (base:debug "meta/receive" behavior message)
      (base:reply sender (behavior message))
      (base:become base-mirror))))))

(define base-mirror-actor (base:base/create base-mirror))
            
(define-syntax create-with-mirror 
  (syntax-rules () 
    ((create-with-mirror mirror behavior arguments ...)
     (base:create/f behavior (list arguments ...) mirror))))

(define-syntax create 
  (syntax-rules ()
    ((create behavior arguments ...)
     (meta-create behavior (list arguments ...)))))
