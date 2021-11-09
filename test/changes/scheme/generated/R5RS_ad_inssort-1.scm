; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((insertion-sort (lambda (vector)
                           (let ((high (- (vector-length vector) 1)))
                              (letrec ((shift-left (lambda (vector index)
                                                     (vector-set! vector (- index 1) (vector-ref vector index))))
                                       (insert-sort-iter (lambda (index1)
                                                           (letrec ((insert (lambda (index1)
                                                                              (let ((insert-value (vector-ref vector (- index1 1))))
                                                                                 (<change>
                                                                                    ()
                                                                                    (display insert-iter))
                                                                                 (letrec ((insert-iter (lambda (index2)
                                                                                                         (if (if (<change> (<= index2 high) (not (<= index2 high))) (< (vector-ref vector index2) insert-value) #f)
                                                                                                            (begin
                                                                                                               (shift-left vector index2)
                                                                                                               (<change>
                                                                                                                  (insert-iter (+ index2 1))
                                                                                                                  ((lambda (x) x) (insert-iter (+ index2 1)))))
                                                                                                            (vector-set! vector (- index2 1) insert-value)))))
                                                                                    (insert-iter index1))))))
                                                              (if (> index1 0)
                                                                 (begin
                                                                    (<change>
                                                                       (insert index1)
                                                                       (insert-sort-iter (- index1 1)))
                                                                    (<change>
                                                                       (insert-sort-iter (- index1 1))
                                                                       (insert index1)))
                                                                 #f)))))
                                 (insert-sort-iter high)))))
         (vect (vector 5 2 7 1 0 9 8 6 3 4)))
   (insertion-sort vect)
   (equal? vect (vector 0 1 2 3 4 5 6 7 8 9)))