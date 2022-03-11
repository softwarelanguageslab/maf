;; TODO local store version does not terminate on this one it seems

(define (ack m n)
  (cond
   ((= m 0) (+ n 1))
   ((= n 0) (ack (- m 1) 1))
   (else (ack (- m 1) (ack m (- n 1))))))

(provide/contract (ack (-> number? number? number?)))
(@unchecked ack OPQ OPQ)
(safe)
