(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Simplified numbered?
(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          (else (and (numbered? (car aexp))
                     (numbered? (car (cdr (cdr aexp)))))))))

(define ^
  (lambda (n m)
    (cond ((zero? m) 1)
          (else (* n (^ n (- m 1)))))))

;; evaluate for infix expression
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (car (cdr nexp)) '+)
           (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp)) '*)
           (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
          (else
           (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))
(value '((5 ^ 1) * (3 + 3)))