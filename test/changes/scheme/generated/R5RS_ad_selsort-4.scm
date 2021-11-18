; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((selection-sort (lambda (vector)
                           (letrec ((swap (lambda (vector index1 index2)
                                            (let ((temp (vector-ref vector index1)))
                                               (vector-set! vector index1 (vector-ref vector index2))
                                               (<change>
                                                  ()
                                                  vector-set!)
                                               (vector-set! vector index2 temp))))
                                    (pos-of-min (lambda (vector low high)
                                                  (letrec ((min-iter (lambda (index pos-of-min-so-far)
                                                                       (if (<= index high)
                                                                          (if (< (vector-ref vector index) (vector-ref vector pos-of-min-so-far))
                                                                             (min-iter (+ index 1) index)
                                                                             (min-iter (+ index 1) pos-of-min-so-far))
                                                                          pos-of-min-so-far))))
                                                     (min-iter (+ low 1) low)))))
                              (let ((high (- (vector-length vector) 1)))
                                 (letrec ((selection-sort-iter (lambda (index)
                                                                 (if (< index high)
                                                                    (begin
                                                                       (<change>
                                                                          (swap vector index (pos-of-min vector index high))
                                                                          (selection-sort-iter (+ index 1)))
                                                                       (<change>
                                                                          (selection-sort-iter (+ index 1))
                                                                          (swap vector index (pos-of-min vector index high))))
                                                                    #f))))
                                    (selection-sort-iter 0))))))
         (vect (vector 5 7 0 9 6 4 3 8 2 1)))
   (<change>
      (selection-sort vect)
      ((lambda (x) x) (selection-sort vect)))
   (equal? vect (vector 0 1 2 3 4 5 6 7 8 9)))