;; TODO see ex-02

(define (f x y)
  (if (and (number? x) (string? y))
      (+ x (string-length y))
      (string-length x)))

(provide/contract 
  (f (-> (or/c number? string?) string? number?)))

(@unchecked f OPQ OPQ)
(safe)
