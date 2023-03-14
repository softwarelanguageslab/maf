; Changes:
; * removed: 0
; * added: 6
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
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
                         heap)
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
                                                   (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                                      (begin
                                                         (swap heap child parent)
                                                         (<change>
                                                            ()
                                                            (display (iter child)))
                                                         (iter child))
                                                      #f)
                                                   #f)))))))
                         (iter from))))
         (swap (lambda (a-vector i1 i2)
                 (let ((temp (vector-ref a-vector i1)))
                    (vector-set! a-vector i1 (vector-ref a-vector i2))
                    (vector-set! a-vector i2 temp))))
         (sift-up (lambda (heap from)
                    (letrec ((iter (lambda (child)
                                     (let ((parent (quotient child 2)))
                                        (if (> parent 0)
                                           (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                              (begin
                                                 (<change>
                                                    (swap heap child parent)
                                                    ((lambda (x) x) (swap heap child parent)))
                                                 (iter parent))
                                              #f)
                                           #f)))))
                       (iter from))))
         (create-heap (lambda (size)
                        (<change>
                           ()
                           (my-++ size))
                        (cons 0 (make-vector (my-++ size)))))
         (is-empty? (lambda (heap)
                      (<change>
                         ()
                         (car heap))
                      (eq? (car heap) 0)))
         (insert (lambda (heap item)
                   (<change>
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
                         (newline))
                      ((lambda (x) x)
                         (let* ((content (cdr heap))
                                (new-nr-of-elements (my-++ (car heap)))
                                (size (my--- (vector-length content))))
                            (display "insert    ")
                            (if (> new-nr-of-elements size)
                               false
                               (begin
                                  (<change>
                                     (vector-set! content new-nr-of-elements item)
                                     ((lambda (x) x) (vector-set! content new-nr-of-elements item)))
                                  (sift-up content new-nr-of-elements)
                                  (set-car! heap new-nr-of-elements)))
                            (<change>
                               ()
                               (display vector-set!))
                            (<change>
                               (display heap)
                               (newline))
                            (<change>
                               (newline)
                               (display heap)))))))
         (v (vector 'lol 5 8 1 3 9 10 2 0)))
   (<change>
      ()
      'lol)
   (make-heap v 8)
   (equal? v (vector 'lol 0 3 1 5 9 10 2 8)))