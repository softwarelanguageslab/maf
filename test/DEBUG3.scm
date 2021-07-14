; icp_3_leval_ex_5
(define lazy? (<change> #f (lambda (v) (tagged-list? v 'lazy)))) ; <====================================================
(define memo? (<change> #f (lambda (v) (tagged-list? v 'lazy-memo)))) ; <===============================================
(define tagged-declaration? (<change> #f pair?)) ; <====================================================================
(define parameter-name (<change> #f (lambda (v) (if (tagged-declaration? v) (cadr v) v)))) ; <==========================
(define thunk-memoizable? (<change> #f (lambda (v) (tagged-list? v 'thunk-memoizable)))) ; <============================

;;
;;toegevoegd
;;
(define true #t)
(define false #f)

;;
;; zie deel 3 p6
;;
(define delay-it
  (<change> ; <=========================================================================================================
    (lambda (exp env) (eval exp env))
    (lambda (decl exp env)
      (cond ((not (tagged-declaration? decl))
             ; Geen tag: 'gewoon' applicative-order evaluatie
             (eval exp env))
            ((lazy? decl)
             (eval exp env))
            ((memo? decl)
             (eval exp env))))))

;;
;; ;; zie deel 3 p16
;;
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

;;
;; zie deel 1.1 p52
;;
(define (apply-in-underlying-scheme proc args)
  (cond
   ((null? args) (proc))
   ((null? (cdr args)) (proc (car args)))
   ((null? (cddr args)) (proc (car args) (cadr args)))
   (else (error "Unsupported call"))))

;;
;; zie deel 3 p9 en p24
;;
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((application? exp) ;;aangepast
         (leval-apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL"))))

;;
;; zie deel 3 p7
;;
(define (actual-value exp env)
  (eval exp env))

;;
;; zie deel 3 p10
;;

(define (leval-apply procedure arguments env) ;; aangepast
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ;; aangepast
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (<change> ; <=================================================================================================
            (extend-environment
              (procedure-parameters procedure)
              (list-of-delayed-args arguments env) ;; aangepast
              (procedure-environment procedure))
            (let ((params (procedure-parameters procedure)))
              (extend-environment
                ; Namen uit parameter-lijst halen:
                (map parameter-name params)
                ; Parameters (definities) meegeven:
                (list-of-delayed-args params arguments env)
                (procedure-environment procedure))))))
        (else
         (error
          "Unknown procedure type -- APPLY"))))

;;
;; zie deel 3 p11
;;
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define list-of-delayed-args
  (<change> ; <=========================================================================================================
    (lambda (exps env)
      (if (no-operands? exps)
        '()
        (cons (delay-it (first-operand exps) env)
              (list-of-delayed-args (rest-operands exps)
                                    env))))
    (lambda (var-decls exps env)
      (if (no-operands? exps)
          '()
          (cons (delay-it (car var-decls)
                  (first-operand exps) env)
            (list-of-delayed-args (cdr var-decls)
              (rest-operands exps)
              env))))))

;;
;; zie deel 1.1 p36
;;
(define (eval-sequence exps env)
  (cond ((null? (cdr exps)) (eval (car exps) env))
        (else (eval (car exps) env)
              (eval-sequence (cdr exps) env))))

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

;;
;; zie deel 1.1 p18
;;
(define (variable? exp) (symbol? exp))

;;
;; zie deel 1.1 p21
;;
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

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
;; zie deel 1.1 p29
;;
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

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
          (error "Too many arguments supplied")
          (error "Too few arguments supplied"))))

;;
;; zie deel 1.1 p44
;;
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

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
        (error (string-append "Unbound variable " (symbol->string var)))
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;
;; zie deel 1.1 p50
;;
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    initial-env))

;;
;; zie deel 1.1 p51
;;
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list
        (list 'cdr cdr)
        (list 'null (lambda () '()))
        (list 'cons cons)))

;;
;; zie deel 1.1 p52
;;
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define the-global-environment (setup-environment))

(eval '(cdr (cons 13 (cons 14 (cons 15 (null)))))
      the-global-environment)