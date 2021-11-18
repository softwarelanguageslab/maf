; Changes:
; * removed: 1
; * added: 2
; * swaps: 2
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((my-++ (lambda (n)
                  (+ n 1)))
         (my--- (lambda (n)
                  (- n 1)))
         (false #f)
         (true #t)
         (nil ())
         (key (lambda (x)
                x))
         (make-heap (lambda (a-vector nr-of-elements)
                      (letrec ((iter (lambda (index)
                                       (if (> index 0)
                                          (begin
                                             (<change>
                                                ()
                                                (display iter))
                                             (<change>
                                                (sift-down a-vector index nr-of-elements)
                                                (iter (my--- index)))
                                             (<change>
                                                (iter (my--- index))
                                                (sift-down a-vector index nr-of-elements)))
                                          #f))))
                         (iter (quotient nr-of-elements 2)))))
         (sift-down (lambda (heap from to)
                      (letrec ((smallest-child (lambda (parent)
                                                 (let* ((child1 (* 2 parent))
                                                        (child2 (my-++ child1)))
                                                    (if (> child1 to)
                                                       false
                                                       (if (> child2 to)
                                                          (<change>
                                                             child1
                                                             (if (< (key (vector-ref heap child1)) (key (vector-ref heap child2)))
                                                                child1
                                                                child2))
                                                          (<change>
                                                             (if (< (key (vector-ref heap child1)) (key (vector-ref heap child2)))
                                                                child1
                                                                child2)
                                                             child1))))))
                               (iter (lambda (parent)
                                       (let ((child (smallest-child parent)))
                                          (if child
                                             (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                                (begin
                                                   (swap heap child parent)
                                                   (iter child))
                                                #f)
                                             #f)))))
                         (iter from))))
         (swap (lambda (a-vector i1 i2)
                 (let ((temp (vector-ref a-vector i1)))
                    (<change>
                       (vector-set! a-vector i1 (vector-ref a-vector i2))
                       (vector-set! a-vector i2 temp))
                    (<change>
                       (vector-set! a-vector i2 temp)
                       (vector-set! a-vector i1 (vector-ref a-vector i2))))))
         (sift-up (lambda (heap from)
                    (letrec ((iter (lambda (child)
                                     (let ((parent (quotient child 2)))
                                        (if (> parent 0)
                                           (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                              (begin
                                                 (swap heap child parent)
                                                 (iter parent))
                                              #f)
                                           #f)))))
                       (<change>
                          ()
                          iter)
                       (iter from))))
         (create-heap (lambda (size)
                        (cons 0 (make-vector (my-++ size)))))
         (is-empty? (lambda (heap)
                      (eq? (car heap) 0)))
         (insert (lambda (heap item)
                   (let* ((content (cdr heap))
                          (new-nr-of-elements (my-++ (car heap)))
                          (size (my--- (vector-length content))))
                      (display "insert    ")
                      (if (> new-nr-of-elements size)
                         false
                         (begin
                            (vector-set! content new-nr-of-elements item)
                            (sift-up content new-nr-of-elements)
                            (set-car! heap new-nr-of-elements)))
                      (display heap)
                      (newline))))
         (v (vector 'lol 5 8 1 3 9 10 2 0)))
   (<change>
      (make-heap v 8)
      ())
   (equal? v (vector 'lol 0 3 1 5 9 10 2 8)))