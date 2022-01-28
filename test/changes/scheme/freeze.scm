; freeze (ICP1)

; Voeg aan de meta-circulaire evaluator van Scheme de special form `freeze` toe a.d.h.v. een predikaat `freeze?` en een procedure `eval-freeze`.
; Als je deze operatie toepast op een variabele, wordt het onmogelijk om die variabele destructief aan te passen.
; Bijvoorbeeld, na een `(freeze a)` op een bestaande variabele `a` krijg je een foutmelding bij `(set! a 42)`.
; Let op dat de scoping regels gerespecteerd blijven.

;Wijzigingen aan de evaluator:
; - procedure freeze? toegevoegd,
; - procedure eval-freeze toegevoegd,
; - extra tak in conditional van eval-procedure: ((freeze? exp) (eval-freeze exp env))
; - omgevingsmodel gewijzigd: i.p.v. cons-cel waarvan de car een lijst van variabele-namen is, en de cdr een lijst van waarden, staat er nu een vector met idx 0 = lijst van variabele-namen, idx 1 = lijst van waarden, idx 2 = booleans die aangeven of de variabele frozen is,
; - procedure freeze-variable! toegevoegd.

; Hernoemd: apply -> apply-fun; apply-in-underlying-scheme -> apply (removed)

;(#%require (only racket/base
;                 time error))

;;
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
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((<change> #f (freeze? exp)) (eval-freeze exp env))
        ((application? exp)
         (apply-fun (eval (operator exp) env)
                    (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;
;; zie deel 1.1 p34/p52
;;
(define (apply-fun procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;;
;; zie deel 1.1 p33
;;
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;
;; zie deel 1.1 p21
;;
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;;
;; zie deel 1.1 p22
;;
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;
;; zie deel 1.1 p24
;;
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (eval-if exp env)
  (cond ((true? (eval (if-predicate exp) env))
         (eval (if-consequent exp) env))
        (else (eval (if-alternative exp) env))))


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
;; zie deel 1.1 p21
;;
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

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
;; zie deel 1.1 p24
;;
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;
;; zie deel 1.1 p28
;;
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;
;; zie deel 1.1 p38/40
;;
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;
;; zie deel 1.1 p42
;;
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

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

;;
;; zie deel 1.1 p48
;;
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define scan
      (<change> (lambda (vars vals)
                  (cond ((null? vars)
                         (env-loop (enclosing-environment env)))
                        ((eq? var (car vars))
                         (set-car! vals val))
                        (else (scan (cdr vars) (cdr vals)))))
                (lambda (vars vals frozen?s)
                  (cond ((null? vars)
                         (env-loop (enclosing-environment env)))
                        ((eq? var (car vars))
                         (if (car frozen?s)
                             (error "Trying to assign a frozen variable -- SET!" var)
                             (set-car! vals val)))
                        (else (scan (cdr vars) (cdr vals) (cdr frozen?s)))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (<change>
           (scan (frame-variables frame)
                 (frame-values frame))
           (scan (frame-variables frame)
                 (frame-values frame)
                 (frame-frozen?s frame))))))
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
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '- -)
        (list '< <)
        (list '> >)
        (list 'display display)
        (list 'list list)
        ;; more primitives
        ))

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

;;
;; zie deel 1.1 p11
;;
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<env>))
      (display object)))

(define the-global-environment (setup-environment))

; Added input from example.
(define input '((define a 2)
                (define b 3)
                (define c 4)
                (freeze a)
                (freeze c)
                (define (set-fn! x y)
                  (define a 0)
                  (set! a x)
                  (set! b y)
                  (list a b c))
                (set-fn! 0 1)
                (begin ; The following is incorrect.
                  (freeze b)
                  (set-fn! 5 6))))
(for-each (lambda (in)
            (let ((output (eval in the-global-environment)))
              (announce-output output-prompt)
              (user-print output)))
          input)