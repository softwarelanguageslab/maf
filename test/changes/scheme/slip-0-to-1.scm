;
; Slip: Lisp in 100 lines - Theo D'Hondt:SOFT: VUB - 2010
;
;       Simple recursive version in Scheme
;
;       Version 0 - basic facilities
;
; <expression>  ::= <computation>|<lambda>|<variable>|
;                   <literal>|<null>
; <computation> ::= <definition>|<assignment>|<sequence>|
;                   <conditional>|<application>
; <definition>  ::= (define <variable> <expression>)
; <assignment>  ::= (set! <variable> <expression>)
; <sequence>    ::= (begin <expression>+)
; <conditional> ::= (if <expression> <expression> <expression>)
; <application> ::= (<expression>+)
; <lambda>      ::= (lambda (<variable>*) <expression>)
; <variable>    ::= [symbol]
; <literal>     ::= [number]|[character]|[string]|#t|#f
; <null>        ::= ()

;; Changes: from Slip version 0 to Slip version 1.
;; Also added input + changed evaluate-variable to work in maf.

(begin
  (define environment '())
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
      (for-each bind-variable parameters arguments))

    (define (evaluate-sequence expressions)
      (define head (car expressions))
      (define tail (cdr expressions))
      (let ((value (evaluate head)))
        (if (null? tail)
            value
            (evaluate-sequence tail))))

    (define make-procedure
      (<change> (lambda (parameters expression)
                  (define lexical-scope environment)
                  (lambda arguments
                    (define dynamic-scope environment)
                    (set! environment lexical-scope)
                    (bind-parameters parameters arguments)
                    (let ((value (evaluate expression)))
                      (set! environment dynamic-scope)
                      value)))
                (lambda (parameters expressions)
                  (define lexical-scope environment)
                  (lambda arguments
                    (define dynamic-scope environment)
                    (set! environment lexical-scope)
                    (bind-parameters parameters arguments)
                    (let ((value (evaluate expressions)))
                      (set! environment dynamic-scope)
                      value)))))

    ;
    ; evaluation functions
    ;

    (define (evaluate-application operator)
      (lambda operands
        (apply (evaluate operator) (map evaluate operands))))

    (define (evaluate-begin . expressions)
      (evaluate-sequence expressions))

    (define evaluate-define
      (<change>
       (lambda (variable expression)
         (define binding (cons variable '()))
         (set! environment (cons binding environment))
         (let ((value (evaluate expression)))
           (set-cdr! binding value)
           value))
       (lambda (pattern . expressions)
         (define binding (cons pattern '()))
         (set! environment (cons binding environment))
         (if (symbol? pattern)
             (let ((value (evaluate (car expressions))))
               (set-cdr! binding value)
               value)
             (let ((procedure (make-procedure (cdr pattern) expressions)))
               (set-car! binding (car pattern))
               (set-cdr! binding procedure)
               procedure)))))

    (define (evaluate-if predicate consequent alternative)
      (define boolean (evaluate predicate))
      (if (eq? boolean #f)
          (evaluate alternative)
          (evaluate consequent)))

    (define evaluate-lambda
      (<change>
       (lambda (parameters expression)
         (make-procedure parameters expression))
       (lambda (parameters . expressions)
         (make-procedure parameters expressions))))

    (define evaluate-quote
      (<change>
       #f
       (lambda (expression) expression)))

    (define (evaluate-set! variable expression)
      (define binding (assoc variable environment))
      (if binding
          (let ((value (evaluate expression)))
            (set-cdr! binding value)
            value)
          (error "inaccessible variable: " variable)))

    (define (evaluate-variable variable)
      (define binding (assoc variable environment))
      (cond (binding (cdr binding))
            ((eq? variable '+) +)
            (else (eval variable (interaction-environment))))) ;; Doesn't work in MAF.

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
              ((<change> #f (eqv? operator 'quote)) evaluate-quote)
              ((eqv? operator 'set!)   evaluate-set!  )
              (else                    (evaluate-application operator))) operands)))
        (else
         expression)))

    ;
    ; read-eval-print
    ;

    (display output)
    (newline)
    (display ">>>")
    (loop (evaluate (mread))))

  (define user-inputs '((define x 20)
                        (define y (begin (set! x (+ x 1)) x))
                        (define z (lambda (a) y))
                        (begin y '5)))
  (define (mread)
    (if (null? user-inputs)
        (error "No more user inputs.")
        (let ((first (car user-inputs)))
          (set! user-inputs (cdr user-inputs))
          first)))

  (loop (<change> "Slip version 0" "Slip version 1")))