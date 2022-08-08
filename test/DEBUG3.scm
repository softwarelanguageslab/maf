
;;toegevoegd
;;
(define true #t)
(define false #f)

;;
;; zie deel 1.1 p52
;;
;(define apply-in-underlying-scheme apply)

(define freeze?
  (<change>
    #f
    (lambda (exp)
      (tagged-list? exp 'freeze))))
(define eval-freeze
  (<change>
    #f
    (lambda (exp env)
      (let ((var (cadr exp)))
        (freeze-variable! var env)
        'done))))

;;
;; zie deel 1.1 p16
;;
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((definition? exp) (eval-definition exp env))
    ((begin? exp)
      (eval-sequence (begin-actions exp) env))
    (else
      (error "Unknown expression type -- EVAL" exp))))


;;
;; zie deel 1.1 p33
;;
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
      (list-of-values (rest-operands exps) env))))

;;
;; zie deel 1.1 p22
;;
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


;;
;; zie deel 1.1 p36
;;
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
    (else (eval (first-exp exps) env)
      (eval-sequence (rest-exps exps) env))))

;;
;; zie deel 1.1 p18
;;
(define (self-evaluating? exp)
  (cond ((number? exp) true)
    ((string? exp) true)
    (else false)))

;;
;; zie deel 1.1 p19
;;
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;;
;; zie deel 1.1 p18
;;
(define (variable? exp) (symbol? exp))

;;
;; zie deel 1.1 p22
;;
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
      (cddr exp))))

;;
;; zie deel 1.1 p32
;;
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;
;; zie deel 1.1 p44
;;
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;
;; zie deel 1.1 p45
;;
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

;;
;; zie deel 1.1 p44
;;
(define (make-frame variables values)
  (<change>
    (cons variables values)
    (vector variables
      values
      (map (lambda (any) #f) values))))

(define (frame-variables frame) (<change> (car frame) (vector-ref frame 0)))
(define (frame-values frame) (<change> (cdr frame) (vector-ref frame 1)))
(define frame-frozen?s (<change> #f (lambda (frame) (vector-ref frame 2))))

(define (add-binding-to-frame! var val frame)
  (<change> (begin ; Added `begin`s to accomodate change expression.
              (set-car! frame (cons var (car frame)))
              (set-cdr! frame (cons val (cdr frame))))
    (begin
      (vector-set! frame 0 (cons var (frame-variables frame)))
      (vector-set! frame 1 (cons val (frame-values frame)))
      (vector-set! frame 2 (cons #f (frame-frozen?s frame))))))

;;
;; zie deel 1.1 p46
;;
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
              (env-loop (enclosing-environment env)))
        ((eq? var (car vars))
          (car vals))
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
          (frame-values frame)))))
  (env-loop env))

(define freeze-variable!
  (<change> #f
    (lambda (var env)
      (define (env-loop env)
        (define (scan vars frozen?s)
          (cond ((null? vars)
                  (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
              (set-car! frozen?s #t))
            (else (scan (cdr vars) (cdr frozen?s)))))
        (if (eq? env the-empty-environment)
          (error "Unbound variable -- FREEZE!" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
              (frame-frozen?s frame)))))
      (env-loop env))))

;;
;; zie deel 1.1 p49
;;
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
              (add-binding-to-frame! var val frame))
        ((eq? var (car vars))
          (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
      (frame-values frame))))

;;
;; zie deel 1.1 p50
;;
(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
            (primitive-procedure-objects)
            the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;;
;; zie deel 1.1 p51
;;
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list))

(define (primitive-procedure-names)
  (map car
    primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
    primitive-procedures))

;;
;; zie deel 1.1 p52
;;
(define (apply-primitive-procedure proc args)
  (apply ;-in-underlying-scheme
    (primitive-implementation proc) args))

(define the-global-environment (setup-environment))

(for-each (lambda (in)
              (display (eval in the-global-environment)))
  '((define a 2)
     (freeze a)))