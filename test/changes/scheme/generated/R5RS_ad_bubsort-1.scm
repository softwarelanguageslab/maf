; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
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
                                               (<change>
                                                  ()
                                                  vector-set!)
                                               (let ((temp (vector-ref vector index1)))
                                                  (vector-set! vector index1 (vector-ref vector index2))
                                                  (vector-set! vector index2 temp))))
                                       (bubble (lambda (index)
                                                 (<change>
                                                    ()
                                                    #t)
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
                                                              (<change>
                                                                 (if (bubble index)
                                                                    (bubble-sort-iter (- index 1))
                                                                    #f)
                                                                 #f)
                                                              (<change>
                                                                 #f
                                                                 (if (bubble index)
                                                                    (bubble-sort-iter (- index 1))
                                                                    #f))))))
                                 (bubble-sort-iter (- (vector-length vector) 2)))))))
         (vect (vector 9 5 1 7 8 9 4 6 2 3)))
   (bubble-sort vect)
   (equal? vect (vector 1 2 3 4 5 6 7 8 9 9)))