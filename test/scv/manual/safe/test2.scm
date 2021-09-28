;; tests path conditions, should also be safe because the monitor is only applied 
;; when the real? check has passed.

(define real/c (flat real?))
(define x (fresh)) ;; (fresh) generates an opaque value that receives a fresh symbolic representation
(if (real? x)
    (mon real?/c x)
    x)
