(define environment '())
(define rollback environment)

(define (evaluate-application operator)
  (lambda operands
    (apply (evaluate operator) (map evaluate operands))))

(define evaluate-define
  (<change>
   (lambda (variable expression)
     (define binding (cons variable '()))
     (set! environment (cons binding environment))
     #t)
   (lambda (pattern . expressions)
     (define binding (cons pattern '()))
     (set! environment (cons binding environment))
     (if (symbol? pattern)
         #t
         (let ((procedure #t))
           procedure)))))

(define (evaluate-set! variable expression)
  (define binding (assoc variable environment))
  (set! environment rollback))

(define (evaluate expression)
  (cond
    ((symbol? expression)
     (cdr (assoc expression environment)))
    ((pair? expression)
     (let ((operator (car expression))
           (operands (cdr expression)))
       (apply
        (cond ;; Changed from case to cond.
          ((eq? operator 'define) evaluate-define)
          ((eq? operator 'set!)   evaluate-set!)
          (else                    (evaluate-application operator))) operands)))))

(evaluate '((define x 20)))