; Changes:
; * removed: 2
; * added: 3
; * swaps: 3
; * negated predicates: 2
; * swapped branches: 2
; * calls to id fun: 4
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
                                             (sift-down a-vector index nr-of-elements)
                                             (iter (my--- index)))
                                          #f))))
                         (iter (quotient nr-of-elements 2)))))
         (sift-down (lambda (heap from to)
                      (<change>
                         ()
                         child1)
                      (<change>
                         (letrec ((smallest-child (lambda (parent)
                                                    (let* ((child1 (* 2 parent))
                                                           (child2 (my-++ child1)))
                                                       (if (> child1 to)
                                                          false
                                                          (if (> child2 to)
                                                             child1
                                                             (if (< (key (vector-ref heap child1)) (key (vector-ref heap child2)))
                                                                child1
                                                                child2))))))
                                  (iter (lambda (parent)
                                          (let ((child (smallest-child parent)))
                                             (if child
                                                (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                                   (begin
                                                      (swap heap child parent)
                                                      (iter child))
                                                   #f)
                                                #f)))))
                            (iter from))
                         ((lambda (x) x)
                            (letrec ((smallest-child (lambda (parent)
                                                       (let* ((child1 (* 2 parent))
                                                              (child2 (my-++ child1)))
                                                          (if (> child1 to)
                                                             false
                                                             (if (> child2 to)
                                                                child1
                                                                (if (<change> (< (key (vector-ref heap child1)) (key (vector-ref heap child2))) (not (< (key (vector-ref heap child1)) (key (vector-ref heap child2)))))
                                                                   child1
                                                                   child2))))))
                                     (iter (lambda (parent)
                                             (<change>
                                                (let ((child (smallest-child parent)))
                                                   (if child
                                                      (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                                         (begin
                                                            (swap heap child parent)
                                                            (iter child))
                                                         #f)
                                                      #f))
                                                ((lambda (x) x)
                                                   (let ((child (smallest-child parent)))
                                                      (if child
                                                         (<change>
                                                            (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                                               (begin
                                                                  (swap heap child parent)
                                                                  (iter child))
                                                               #f)
                                                            #f)
                                                         (<change>
                                                            #f
                                                            (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                                               (begin
                                                                  (swap heap child parent)
                                                                  (iter child))
                                                               #f)))))))))
                               (iter from))))))
         (swap (lambda (a-vector i1 i2)
                 (<change>
                    (let ((temp (vector-ref a-vector i1)))
                       (vector-set! a-vector i1 (vector-ref a-vector i2))
                       (vector-set! a-vector i2 temp))
                    ((lambda (x) x)
                       (let ((temp (vector-ref a-vector i1)))
                          (<change>
                             ()
                             temp)
                          (<change>
                             (vector-set! a-vector i1 (vector-ref a-vector i2))
                             (vector-set! a-vector i2 temp))
                          (<change>
                             (vector-set! a-vector i2 temp)
                             (vector-set! a-vector i1 (vector-ref a-vector i2))))))))
         (sift-up (lambda (heap from)
                    (letrec ((iter (lambda (child)
                                     (let ((parent (quotient child 2)))
                                        (if (> parent 0)
                                           (<change>
                                              (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                                 (begin
                                                    (swap heap child parent)
                                                    (iter parent))
                                                 #f)
                                              #f)
                                           (<change>
                                              #f
                                              (if (not (> (key (vector-ref heap parent)) (key (vector-ref heap child))))
                                                 (begin
                                                    (iter parent)
                                                    (swap heap child parent))
                                                 #f)))))))
                       (iter from))))
         (create-heap (lambda (size)
                        (cons 0 (make-vector (my-++ size)))))
         (is-empty? (lambda (heap)
                      (eq? (car heap) 0)))
         (insert (lambda (heap item)
                   (<change>
                      ()
                      size)
                   (let* ((content (cdr heap))
                          (new-nr-of-elements (my-++ (car heap)))
                          (size (my--- (vector-length content))))
                      (display "insert    ")
                      (if (> new-nr-of-elements size)
                         false
                         (begin
                            (<change>
                               (vector-set! content new-nr-of-elements item)
                               (sift-up content new-nr-of-elements))
                            (<change>
                               (sift-up content new-nr-of-elements)
                               (vector-set! content new-nr-of-elements item))
                            (set-car! heap new-nr-of-elements)))
                      (<change>
                         (display heap)
                         ())
                      (<change>
                         (newline)
                         ((lambda (x) x) (newline))))))
         (v (vector 'lol 5 8 1 3 9 10 2 0)))
   (<change>
      (make-heap v 8)
      ())
   (equal? v (vector 'lol 0 3 1 5 9 10 2 8)))