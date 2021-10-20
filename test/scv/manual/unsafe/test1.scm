; sanity check: do flat contracts work correctly on concrete values

(define number?/c (flat number?))
(define boolean?/c (flat boolean?))

(define foo 
  (mon (-> number?/c boolean?/c) (lambda (x) (+ x 1))))

(foo 5)
