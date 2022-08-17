#lang racket

(require (prefix-in base: "./meta-base.rkt"))
(provide (all-defined-out))

(define send-meta base:actor-send)

(define (meta-create behavior arguments line column source . mirror)
  (base:debug "meta-create" behavior arguments mirror)
  (base:debug "mirror:" (base:current-mirror))
  (if (base:current-mirror)
    (base:with-increased-meta
       (base:await 
         (base:ask (base:current-mirror) create behavior 
                   arguments line column source (if (null? mirror) #f (car mirror)))))
    (apply base:create/f behavior arguments line column source mirror)))

(define base-mirror 
  (base:behavior "base-mirror" ((delegator any?)) 
   (base:messages 
     ((send (sender receiver tag arguments)
      ;(base:debug "meta/send:" sender receiver tag arguments)
      (base:reply sender (base:actor-send receiver (base:message tag arguments)))
      (base:become delegator delegator))
     (create (sender behavior arguments line column source mirror)
      ;(base:debug "meta/create" behavior arguments)
      (base:reply sender 
         (apply base:create/f behavior arguments line column source (if mirror (list mirror) '())))
      (base:become delegator delegator))
     (receive (sender behavior message)
      ;(base:debug "meta/receive" behavior message)
      (base:reply sender (lambda () (behavior message)))
      (base:become delegator delegator))))))

(define base-mirror-actor (create-with-mirror #f base-mirror base-mirror))
            
(define-syntax (create-with-mirror stx)
  (syntax-case stx () 
    ((create-with-mirror mirror behavior arguments ...)
     (let ((line (syntax-line stx))
           (column (syntax-column stx))
           (source (syntax-source stx)))

     #`(meta-create behavior (list arguments ...) #,line #,column #,source mirror)))))

(define-syntax (create stx)
  (syntax-case stx ()
    ((create behavior arguments ...)
     (let ((line (syntax-line stx))
           (column (syntax-column stx))
           (source (syntax-source stx)))

     #`(meta-create behavior (list arguments ...) #,line #,column #,source)))))
