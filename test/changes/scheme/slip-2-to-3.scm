;
; Slip: Lisp in 100 lines - Theo D'Hondt: SOFT: VUB - 2010
;
;       Simple recursive version in Scheme
;
;       Version 2 - extension with variable argument lists
;                   and single-clause if
;
; <expression>  ::= <computation>|<lambda>|<quote>|<variable>|
;                   <literal>|<null>
; <computation> ::= <definition>|<assignment>|<sequence>|
;                   <conditional>|<application>
; <definition>  ::= (define <variable> <expression>)
; <definition>  ::= (define <pattern> <expression>+)
; <assignment>  ::= (set! <variable> <expression>)
; <sequence>    ::= (begin <expression>+)
; <conditional> ::= (if <expression> <expression> <expression>)
; <conditional> ::= (if <expression> <expression>)
; <application> ::= (<expression>+)
; <lambda>      ::= (lambda () <expression>+)
; <lambda>      ::= (lambda <variable> <expression>+)
; <lambda>      ::= (lambda (<pattern>) <expression>+)
; <quote>       ::= '[s-expression]
; <variable>    ::= [symbol]
; <pattern>     ::= (<variable>+)
; <pattern>     ::= (<variable>+ . <variable>)
; <literal>     ::= [number]|[character]|[string]|#t|#f
; <null>        ::= ()

;; Changes: from Slip version 2 to Slip version 3.
;; Also added input (go meta twice) + changed evaluate-variable to work in maf.

(begin
  (define circularity-level (<change> #f 0))
  (define environment '(('+ +) ; Extended environment
                        ('- -)
                        ('* *)
                        ('/ /)
                        ('eq eq)
                        ('symbol? symbol?)
                        ('car car)
                        ('cdr cdr)
                        ('cons cons)
                        ('null null)
                        ('pair pair)
                        ('set-car! set-car!)
                        ('set-cdr! set-cdr!)))

  (define lookup
    (<change> #f
              (lambda (variable)
                (define binding (assoc variable environment))
                (if (pair? binding)
                    (cdr binding)
                    (eval variable (interaction-environment))))))

  (define (loop output)
    (define rollback environment)

    (define (error message qualifier)
      (display message)
      (set! environment rollback)
      (loop qualifier))

    ;
    ; functions
    ;

    (define (bind-variable variable value)
      (define binding (cons variable value))
      (set! environment (cons binding environment)))

    (define (bind-parameters parameters arguments)
      (if (symbol? parameters)
          (bind-variable parameters arguments))
      (if (pair? parameters)
          (let
              ((variable (car parameters))
               (value    (car arguments )))
            (bind-variable variable value)
            (bind-parameters (cdr parameters) (cdr arguments)))))

    (define (evaluate-sequence expressions)
      (define head (car expressions))
      (define tail (cdr expressions))
      (<change>
       (let ((value (evaluate head)))
         (if (null? tail)
             value
             (evaluate-sequence tail)))
       (if (null? tail)
           (evaluate head)
           (begin
             (evaluate head)
             (evaluate-sequence tail)))))

    (define (make-procedure parameters expressions)
      (define lexical-environment environment)
      (lambda arguments
        (define dynamic-environment environment)
        (set! environment lexical-environment)
        (bind-parameters parameters arguments)
        (let ((value (evaluate-sequence expressions)))
          (set! environment dynamic-environment)
          value)))

    ;
    ; evaluation functions
    ;

    (define (evaluate-application operator)
      (lambda operands
        (apply (evaluate operator) (map evaluate operands))))

    (define (evaluate-begin . expressions)
      (evaluate-sequence expressions))

    (define (evaluate-define pattern . expressions)
      (<change>
       (begin
         (define binding (cons pattern '()))
         (set! environment (cons binding environment))
         (if (symbol? pattern)
             (let ((value (evaluate (car expressions))))
               (set-cdr! binding value)
               value)
             (let ((procedure (make-procedure (cdr pattern) expressions)))
               (set-car! binding (car pattern))
               (set-cdr! binding procedure)
               procedure)))
       (begin
         (if (symbol? pattern)
             (let* ((value (evaluate (car expressions)))
                    (binding (cons pattern value)))
               (set! environment (cons binding environment))
               value)
             (let* ((binding (cons (car pattern) '())))
               (set! environment (cons binding environment))
               (let ((procedure (make-procedure (cdr pattern) expressions)))
                 (set-cdr! binding procedure)
                 procedure))))))

    (define (evaluate-if predicate consequent . alternative)
      (define boolean (evaluate predicate))
      (if (eq? boolean #f)
          (if (null? alternative)
              '()
              (evaluate (car alternative)))
          (evaluate consequent)))

    (define (evaluate-lambda parameters . expressions)
      (make-procedure parameters expressions))

    (define (evaluate-quote expression)
      expression)

    (define (evaluate-set! variable expression)
      (define binding (assoc variable environment))
      (if (pair? binding)
          (let ((value (evaluate expression)))
            (set-cdr! binding value)
            value)
          (error "inaccessible variable: " variable)))

    (define (evaluate-variable variable)
      (<change>
       (begin
         (define binding (assoc variable environment))
         (if (pair? binding)
             (cdr binding)
             (eval variable (interaction-environment))))
       (lookup variable)))

    (define evaluate-while
      (<change>
       #f
       (lambda (predicate . expressions)
         (define save-environment environment)
         (define (iterate value)
           (set! environment save-environment)
           (let ((boolean (evaluate predicate)))
             (set! environment save-environment)
             (if (eq? boolean #f)
                 value
                 (iterate (evaluate-sequence expressions)))))
         (iterate '()))))

    ;
    ; evaluator
    ;

    (define (evaluate expression)
      (cond
        ((symbol? expression)
         (evaluate-variable expression))
        ((pair? expression)
         (let ((operator (car expression))
               (operands (cdr expression)))
           (apply
            (cond ;; Changed from case to cond.
              ((eqv? operator 'begin)  evaluate-begin )
              ((eqv? operator 'define) evaluate-define)
              ((eqv? operator 'if)     evaluate-if    )
              ((eqv? operator 'lambda) evaluate-lambda)
              ((eqv? operator 'quote)  evaluate-quote )
              ((eqv? operator 'set!)   evaluate-set!  )
              ((<change> #f (eqv? operator 'while)) evaluate-while)
              (else                    (evaluate-application operator))) operands)))
        (else
         expression)))

    ;
    ; read-eval-print
    ;

    (display output)
    (newline)
    (<change>
     #f
     (begin
       (display "level ")
       (display circularity-level)))
    (display ">>>")
    (loop (evaluate (mread))))

  (define user-inputs '((begin
                          (define circularity-level (+ circularity-level 1))
                          (define environment ())
                          (define meta-lookup lookup)

                          (define (lookup variable)
                            (define binding (assoc variable environment))
                            (if (pair? binding)
                                (cdr binding)
                                (meta-lookup variable)))

                          (define (loop output)
                            (define rollback environment)

                            (define (evaluate expression)

                              (define (error message qualifier)
                                (display message)
                                (set! environment rollback)
                                (loop qualifier))

                              (define (bind-variable variable value)
                                (define binding (cons variable value))
                                (set! environment (cons binding environment)))

                              (define (bind-parameters parameters arguments)
                                (if (symbol? parameters)
                                    (bind-variable parameters arguments)
                                    (if (pair? parameters)
                                        (begin
                                          (define variable (car parameters))
                                          (define value (car arguments ))
                                          (bind-variable variable value)
                                          (bind-parameters (cdr parameters) (cdr arguments))))))

                              (define (evaluate-sequence expressions)
                                (define head (car expressions))
                                (define tail (cdr expressions))
                                (if (null? tail)
                                    (evaluate head)
                                    (begin
                                      (evaluate head)
                                      (evaluate-sequence tail))))

                              (define (make-procedure parameters expressions)
                                (define lexical-environment environment)
                                (lambda arguments
                                  (define dynamic-environment environment)
                                  (set! environment lexical-environment)
                                  (bind-parameters parameters arguments)
                                  (define value (evaluate-sequence expressions))
                                  (set! environment dynamic-environment)
                                  value))

                              (define (evaluate-application operator)
                                (lambda operands
                                  (apply (evaluate operator) (map evaluate operands))))

                              (define (evaluate-begin . expressions)
                                (evaluate-sequence expressions))

                              (define (evaluate-define pattern . expressions)
                                (if (symbol? pattern)
                                    (begin
                                      (define value (evaluate (car expressions)))
                                      (define binding (cons pattern value))
                                      (set! environment (cons binding environment))
                                      value)
                                    (begin
                                      (define binding (cons (car pattern) ()))
                                      (set! environment (cons binding environment))
                                      (define procedure (make-procedure (cdr pattern) expressions))
                                      (set-cdr! binding procedure)
                                      procedure)))

                              (define (evaluate-if predicate consequent . alternative)
                                (define boolean (evaluate predicate))
                                (if (eq? boolean #f)
                                    (if (null? alternative)
                                        ()
                                        (evaluate (car alternative)))
                                    (evaluate consequent)))

                              (define (evaluate-lambda parameters . expressions)
                                (make-procedure parameters expressions))

                              (define (evaluate-quote expression)
                                expression)

                              (define (evaluate-set! variable expression)
                                (define binding (assoc variable environment))
                                (if (pair? binding)
                                    (begin
                                      (define value (evaluate expression))
                                      (set-cdr! binding value)
                                      value)
                                    (error "inaccessible variable: " variable)))

                              (define (evaluate-variable variable)
                                (lookup variable))

                              (define (evaluate-while predicate . expressions)
                                (define (iterate value)
                                  (define boolean (evaluate predicate))
                                  (if (eq? boolean #f)
                                      value
                                      (iterate (evaluate-sequence expressions))))
                                (iterate ()))

                              (if (symbol? expression)
                                  (evaluate-variable expression)
                                  (if (pair? expression)
                                      (begin
                                        (define operator (car expression))
                                        (define operands (cdr expression))
                                        (apply
                                         (if (eq? operator 'begin) evaluate-begin
                                             (if (eq? operator 'define) evaluate-define
                                                 (if (eq? operator 'if) evaluate-if
                                                     (if (eq? operator 'lambda) evaluate-lambda
                                                         (if (eq? operator 'quote) evaluate-quote
                                                             (if (eq? operator 'set!) evaluate-set!
                                                                 (if (eq? operator 'while) evaluate-while
                                                                     (evaluate-application operator))))))))
                                         operands))
                                      expression)))

                            (display output)
                            (newline)
                            (display "level ")
                            (display circularity-level)
                            (display ">")
                            (loop (evaluate (mread))))

                          (loop "Meta-Circular Slip"))
                        (begin
                          (define circularity-level (+ circularity-level 1))
                          (define environment ())
                          (define meta-lookup lookup)

                          (define (lookup variable)
                            (define binding (assoc variable environment))
                            (if (pair? binding)
                                (cdr binding)
                                (meta-lookup variable)))

                          (define (loop output)
                            (define rollback environment)

                            (define (evaluate expression)

                              (define (error message qualifier)
                                (display message)
                                (set! environment rollback)
                                (loop qualifier))

                              (define (bind-variable variable value)
                                (define binding (cons variable value))
                                (set! environment (cons binding environment)))

                              (define (bind-parameters parameters arguments)
                                (if (symbol? parameters)
                                    (bind-variable parameters arguments)
                                    (if (pair? parameters)
                                        (begin
                                          (define variable (car parameters))
                                          (define value (car arguments ))
                                          (bind-variable variable value)
                                          (bind-parameters (cdr parameters) (cdr arguments))))))

                              (define (evaluate-sequence expressions)
                                (define head (car expressions))
                                (define tail (cdr expressions))
                                (if (null? tail)
                                    (evaluate head)
                                    (begin
                                      (evaluate head)
                                      (evaluate-sequence tail))))

                              (define (make-procedure parameters expressions)
                                (define lexical-environment environment)
                                (lambda arguments
                                  (define dynamic-environment environment)
                                  (set! environment lexical-environment)
                                  (bind-parameters parameters arguments)
                                  (define value (evaluate-sequence expressions))
                                  (set! environment dynamic-environment)
                                  value))

                              (define (evaluate-application operator)
                                (lambda operands
                                  (apply (evaluate operator) (map evaluate operands))))

                              (define (evaluate-begin . expressions)
                                (evaluate-sequence expressions))

                              (define (evaluate-define pattern . expressions)
                                (if (symbol? pattern)
                                    (begin
                                      (define value (evaluate (car expressions)))
                                      (define binding (cons pattern value))
                                      (set! environment (cons binding environment))
                                      value)
                                    (begin
                                      (define binding (cons (car pattern) ()))
                                      (set! environment (cons binding environment))
                                      (define procedure (make-procedure (cdr pattern) expressions))
                                      (set-cdr! binding procedure)
                                      procedure)))

                              (define (evaluate-if predicate consequent . alternative)
                                (define boolean (evaluate predicate))
                                (if (eq? boolean #f)
                                    (if (null? alternative)
                                        ()
                                        (evaluate (car alternative)))
                                    (evaluate consequent)))

                              (define (evaluate-lambda parameters . expressions)
                                (make-procedure parameters expressions))

                              (define (evaluate-quote expression)
                                expression)

                              (define (evaluate-set! variable expression)
                                (define binding (assoc variable environment))
                                (if (pair? binding)
                                    (begin
                                      (define value (evaluate expression))
                                      (set-cdr! binding value)
                                      value)
                                    (error "inaccessible variable: " variable)))

                              (define (evaluate-variable variable)
                                (lookup variable))

                              (define (evaluate-while predicate . expressions)
                                (define (iterate value)
                                  (define boolean (evaluate predicate))
                                  (if (eq? boolean #f)
                                      value
                                      (iterate (evaluate-sequence expressions))))
                                (iterate ()))

                              (if (symbol? expression)
                                  (evaluate-variable expression)
                                  (if (pair? expression)
                                      (begin
                                        (define operator (car expression))
                                        (define operands (cdr expression))
                                        (apply
                                         (if (eq? operator 'begin) evaluate-begin
                                             (if (eq? operator 'define) evaluate-define
                                                 (if (eq? operator 'if) evaluate-if
                                                     (if (eq? operator 'lambda) evaluate-lambda
                                                         (if (eq? operator 'quote) evaluate-quote
                                                             (if (eq? operator 'set!) evaluate-set!
                                                                 (if (eq? operator 'while) evaluate-while
                                                                     (evaluate-application operator))))))))
                                         operands))
                                      expression)))

                            (display output)
                            (newline)
                            (display "level ")
                            (display circularity-level)
                            (display ">")
                            (loop (evaluate (mread))))

                          (loop "Meta-Circular Slip"))
                        (define x 10)
                        x))
  (define (mread)
    (if (null? user-inputs)
        (error "No more user inputs.")
        (let ((first (car user-inputs)))
          (set! user-inputs (cdr user-inputs))
          first)))

  (loop (<change> "Slip version 2" "Slip version 3")))