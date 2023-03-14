;; result: 103

(define saved (<change> #f '())) ; <== Makes the assertion true.
(define done? #f)

(define (foo x)
    (+ x (call/cc bar)))

(define (bar cnt)
  (assert saved) ; Saved must not be false.
  (set! saved cnt)
  42)

(define (main)
  (let ((res (foo 100)))
    (if done?
        res
        (begin
            (set! done? #t)
            (saved 3)))))

(main)
