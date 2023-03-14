; Changes:
; * removed: 1
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((quick-sort (lambda (vector)
                       (letrec ((swap (lambda (vector index1 index2)
                                        (let ((temp (vector-ref vector index1)))
                                           (<change>
                                              (vector-set! vector index1 (vector-ref vector index2))
                                              ())
                                           (vector-set! vector index2 temp))))
                                (quick-sort-aux (lambda (low high)
                                                  (letrec ((quick-sort-aux-iter (lambda (mid-value from to)
                                                                                  (letrec ((quick-right (lambda (index1)
                                                                                                          (if (< (vector-ref vector index1) mid-value)
                                                                                                             (quick-right (+ index1 1))
                                                                                                             index1)))
                                                                                           (quick-left (lambda (index2)
                                                                                                         (if (> (vector-ref vector index2) mid-value)
                                                                                                            (quick-left (- index2 1))
                                                                                                            index2))))
                                                                                     (let ((index1 (quick-right (+ from 1)))
                                                                                           (index2 (quick-left to)))
                                                                                        (if (< index1 index2)
                                                                                           (begin
                                                                                              (swap vector index1 index2)
                                                                                              (quick-sort-aux-iter mid-value index1 index2))
                                                                                           index2))))))
                                                     (if (< low high)
                                                        (begin
                                                           (if (> (vector-ref vector low) (vector-ref vector high))
                                                              (<change>
                                                                 (swap vector low high)
                                                                 #f)
                                                              (<change>
                                                                 #f
                                                                 (swap vector low high)))
                                                           (<change>
                                                              ()
                                                              vector)
                                                           (let ((mid-index (quick-sort-aux-iter (vector-ref vector low) low high)))
                                                              (swap vector mid-index low)
                                                              (quick-sort-aux low (- mid-index 1))
                                                              (quick-sort-aux (+ mid-index 1) high)))
                                                        #f)))))
                          (quick-sort-aux 0 (- (vector-length vector) 1)))))
         (test1 (vector 7 2 4 6 0 8 5 3 1)))
   (quick-sort test1)
   (letrec ((test2 (vector 8 1 4 9 6 3 5 2 7 0)))
      (quick-sort test2)
      (letrec ((test3 (vector 8 3 6 6 1 5 4 2 9 6)))
         (<change>
            (quick-sort test3)
            (if (equal? test1 (vector 0 1 2 3 4 5 6 7 8))
               (if (equal? test2 (vector 0 1 2 3 4 5 6 7 8 9))
                  (equal? test3 (vector 1 2 3 4 5 6 6 6 8 9))
                  #f)
               #f))
         (<change>
            (if (equal? test1 (vector 0 1 2 3 4 5 6 7 8))
               (if (equal? test2 (vector 0 1 2 3 4 5 6 7 8 9))
                  (equal? test3 (vector 1 2 3 4 5 6 6 6 8 9))
                  #f)
               #f)
            (quick-sort test3)))))