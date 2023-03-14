; Changes:
; * removed: 3
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 2
; * calls to id fun: 2
(letrec ((quick-sort (lambda (vector)
                       (letrec ((swap (lambda (v index1 index2)
                                        (let ((temp (vector-ref v index1)))
                                           (<change>
                                              (vector-set! v index1 (vector-ref v index2))
                                              (vector-set! v index2 temp))
                                           (<change>
                                              (vector-set! v index2 temp)
                                              (vector-set! v index1 (vector-ref v index2))))))
                                (quick-sort-aux (lambda (low high)
                                                  (letrec ((quick-sort-aux-iter (lambda (mid-value from to)
                                                                                  (<change>
                                                                                     ()
                                                                                     <)
                                                                                  (letrec ((quick-right (lambda (index1)
                                                                                                          (if (if (< index1 high) (<change> (< (vector-ref vector index1) mid-value) #f) (<change> #f (< (vector-ref vector index1) mid-value)))
                                                                                                             (quick-right (+ index1 1))
                                                                                                             index1)))
                                                                                           (quick-left (lambda (index2)
                                                                                                         (if (if (> index2 low) (> (vector-ref vector index2) mid-value) #f)
                                                                                                            (quick-left (- index2 1))
                                                                                                            index2))))
                                                                                     (<change>
                                                                                        (let ((index1 (quick-right (+ from 1)))
                                                                                              (index2 (quick-left to)))
                                                                                           (if (< index1 index2)
                                                                                              (begin
                                                                                                 (swap vector index1 index2)
                                                                                                 (quick-sort-aux-iter mid-value index1 index2))
                                                                                              index2))
                                                                                        ((lambda (x) x)
                                                                                           (let ((index1 (quick-right (+ from 1)))
                                                                                                 (index2 (quick-left to)))
                                                                                              (if (< index1 index2)
                                                                                                 (begin
                                                                                                    (swap vector index1 index2)
                                                                                                    (quick-sort-aux-iter mid-value index1 index2))
                                                                                                 index2))))))))
                                                     (if (< low high)
                                                        (let ((middle (quotient (+ low high) 2))
                                                              (pivot-index (+ low 1)))
                                                           (swap vector middle pivot-index)
                                                           (if (> (vector-ref vector pivot-index) (vector-ref vector high))
                                                              (swap vector pivot-index high)
                                                              #f)
                                                           (<change>
                                                              (if (> (vector-ref vector low) (vector-ref vector high))
                                                                 (swap vector low high)
                                                                 #f)
                                                              ())
                                                           (if (< (vector-ref vector pivot-index) (vector-ref vector low))
                                                              (<change>
                                                                 (swap vector pivot-index low)
                                                                 #f)
                                                              (<change>
                                                                 #f
                                                                 (swap vector pivot-index low)))
                                                           (let ((mid-index (quick-sort-aux-iter (vector-ref vector pivot-index) (+ low 1) high)))
                                                              (<change>
                                                                 (swap vector mid-index pivot-index)
                                                                 ((lambda (x) x) (swap vector mid-index pivot-index)))
                                                              (<change>
                                                                 (quick-sort-aux low (- mid-index 1))
                                                                 ())
                                                              (quick-sort-aux (+ mid-index 1) high)))
                                                        #f)))))
                          (quick-sort-aux 0 (- (vector-length vector) 1)))))
         (test3 (vector 8 3 6 6 1 5 4 2 9 6)))
   (<change>
      (quick-sort test3)
      ())
   (equal? test3 (vector 1 2 3 4 5 6 6 6 8 9)))