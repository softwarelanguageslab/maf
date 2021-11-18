; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((bubble-sort (lambda (vector)
                        (<change>
                           (letrec ((swap (lambda (vector index1 index2)
                                            (let ((temp (vector-ref vector index1)))
                                               (vector-set! vector index1 (vector-ref vector index2))
                                               (vector-set! vector index2 temp))))
                                    (bubble (lambda (index)
                                              (letrec ((bubble-iter (lambda (index1 changed)
                                                                      (if (<= index1 index)
                                                                         (begin
                                                                            (if (> (vector-ref vector index1) (vector-ref vector (+ index1 1)))
                                                                               (begin
                                                                                  (swap vector index1 (+ index1 1))
                                                                                  (set! changed #t))
                                                                               #f)
                                                                            (bubble-iter (+ index1 1) changed))
                                                                         changed))))
                                                 (bubble-iter 0 #f))))
                                    (bubble-sort-iter (lambda (index)
                                                        (if (>= index 0)
                                                           (if (bubble index)
                                                              (bubble-sort-iter (- index 1))
                                                              #f)
                                                           #f))))
                              (bubble-sort-iter (- (vector-length vector) 2)))
                           ((lambda (x) x)
                              (letrec ((swap (lambda (vector index1 index2)
                                               (let ((temp (vector-ref vector index1)))
                                                  (vector-set! vector index1 (vector-ref vector index2))
                                                  (vector-set! vector index2 temp))))
                                       (bubble (lambda (index)
                                                 (letrec ((bubble-iter (lambda (index1 changed)
                                                                         (if (<change> (<= index1 index) (not (<= index1 index)))
                                                                            (begin
                                                                               (if (> (vector-ref vector index1) (vector-ref vector (+ index1 1)))
                                                                                  (begin
                                                                                     (swap vector index1 (+ index1 1))
                                                                                     (set! changed #t))
                                                                                  #f)
                                                                               (bubble-iter (+ index1 1) changed))
                                                                            changed))))
                                                    (<change>
                                                       (bubble-iter 0 #f)
                                                       ((lambda (x) x) (bubble-iter 0 #f))))))
                                       (bubble-sort-iter (lambda (index)
                                                           (if (>= index 0)
                                                              (if (bubble index)
                                                                 (bubble-sort-iter (- index 1))
                                                                 #f)
                                                              #f))))
                                 (bubble-sort-iter (- (vector-length vector) 2)))))))
         (vect (vector 9 5 1 7 8 9 4 6 2 3)))
   (<change>
      (bubble-sort vect)
      (equal? vect (vector 1 2 3 4 5 6 7 8 9 9)))
   (<change>
      (equal? vect (vector 1 2 3 4 5 6 7 8 9 9))
      (bubble-sort vect)))