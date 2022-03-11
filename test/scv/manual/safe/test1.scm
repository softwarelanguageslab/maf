;; should be safe because the static analysis can compute using 
;; completely concrete values such that number?/c always succeeds

(define number?/c (flat number?))

(define foo 
  (mon (-> number?/c number?/c) (lambda (x) (+ x 1))))

(foo 5)
