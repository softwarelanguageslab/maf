; Changes:
; * removed: 0
; * added: 2
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 4
(letrec ((copy (lambda (from-vector to-vector from-index to-index)
                 (vector-set! to-vector to-index (vector-ref from-vector from-index))))
         (move (lambda (from-vector to-vector from-low from-high to-index)
                 (letrec ((move-iter (lambda (n)
                                       (if (<= (+ from-low n) from-high)
                                          (begin
                                             (<change>
                                                (copy from-vector to-vector (+ from-low n) (+ to-index n))
                                                (move-iter (+ n 1)))
                                             (<change>
                                                (move-iter (+ n 1))
                                                (copy from-vector to-vector (+ from-low n) (+ to-index n))))
                                          #f))))
                    (move-iter 0))))
         (merge (lambda (vector1 vector2 vector low1 high1 low2 high2 to-index)
                  (<change>
                     ()
                     (display copy))
                  (letrec ((merge-iter (lambda (index index1 index2)
                                         (if (> index1 high1)
                                            (move vector2 vector index2 high2 index)
                                            (if (> index2 high2)
                                               (move vector1 vector index1 high1 index)
                                               (if (< (vector-ref vector1 index1) (vector-ref vector2 index2))
                                                  (begin
                                                     (<change>
                                                        (copy vector1 vector index1 index)
                                                        ((lambda (x) x) (copy vector1 vector index1 index)))
                                                     (<change>
                                                        ()
                                                        index1)
                                                     (merge-iter (+ index 1) (+ index1 1) index2))
                                                  (begin
                                                     (copy vector2 vector index2 index)
                                                     (<change>
                                                        (merge-iter (+ index 1) index1 (+ index2 1))
                                                        ((lambda (x) x) (merge-iter (+ index 1) index1 (+ index2 1)))))))))))
                     (merge-iter to-index low1 low2))))
         (bottom-up-merge-sort (lambda (vector)
                                 (letrec ((merge-subs (lambda (len)
                                                        (let ((aux-vector (make-vector (vector-length vector) 0)))
                                                           (letrec ((merge-subs-iter (lambda (index)
                                                                                       (if (< index (- (vector-length vector) (* 2 len)))
                                                                                          (begin
                                                                                             (merge vector vector aux-vector index (+ index len -1) (+ index len) (+ index len len -1) index)
                                                                                             (move aux-vector vector index (+ index len len -1) index)
                                                                                             (merge-subs-iter (+ index len len)))
                                                                                          (if (< index (- (vector-length vector) len))
                                                                                             (begin
                                                                                                (merge
                                                                                                   vector
                                                                                                   vector
                                                                                                   aux-vector
                                                                                                   index
                                                                                                   (+ index len -1)
                                                                                                   (+ index len)
                                                                                                   (- (vector-length vector) 1)
                                                                                                   index)
                                                                                                (move aux-vector vector index (- (vector-length vector) 1) index))
                                                                                             #f)))))
                                                              (merge-subs-iter 0)))))
                                          (merge-sort-iter (lambda (len)
                                                             (if (< len (vector-length vector))
                                                                (begin
                                                                   (merge-subs len)
                                                                   (<change>
                                                                      (merge-sort-iter (* 2 len))
                                                                      ((lambda (x) x) (merge-sort-iter (* 2 len)))))
                                                                #f))))
                                    (merge-sort-iter 1)))))
   (let ((aVector (vector 8 3 6 6 0 5 4 2 9 6)))
      (<change>
         (bottom-up-merge-sort aVector)
         ((lambda (x) x) (bottom-up-merge-sort aVector)))
      (equal? aVector (vector 0 2 3 4 5 6 6 6 8 9))))