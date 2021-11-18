; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((true #t)
         (false #f)
         (make-item (lambda (priority element)
                      (cons priority element)))
         (get-priority (lambda (item)
                         (car item)))
         (get-element (lambda (item)
                        (<change>
                           ()
                           cdr)
                        (cdr item)))
         (create-priority-queue (lambda ()
                                  (<change>
                                     (let ((front (cons 'boe ())))
                                        (letrec ((content (lambda ()
                                                            (cdr front)))
                                                 (insert-after! (lambda (cell item)
                                                                  (let ((new-cell (cons item ())))
                                                                     (set-cdr! new-cell (cdr cell))
                                                                     (set-cdr! cell new-cell))))
                                                 (find-prev-cell (lambda (priority)
                                                                   (letrec ((find-iter (lambda (rest prev)
                                                                                         (if (null? rest)
                                                                                            prev
                                                                                            (if (> (get-priority (car rest)) priority)
                                                                                               (find-iter (cdr rest) rest)
                                                                                               prev)))))
                                                                      (find-iter (content) front))))
                                                 (empty? (lambda ()
                                                           (null? (content))))
                                                 (enqueue (lambda (priority element)
                                                            (insert-after! (find-prev-cell priority) (make-item priority element))
                                                            true))
                                                 (dequeue (lambda ()
                                                            (if (null? (content))
                                                               false
                                                               (let ((temp (car (content))))
                                                                  (set-cdr! front (cdr (content)))
                                                                  (get-element temp)))))
                                                 (serve (lambda ()
                                                          (if (null? (content))
                                                             false
                                                             (get-element (car (content))))))
                                                 (dispatch (lambda (m)
                                                             (if (eq? m 'empty?)
                                                                empty?
                                                                (if (eq? m 'enqueue)
                                                                   enqueue
                                                                   (if (eq? m 'dequeue)
                                                                      dequeue
                                                                      (if (eq? m 'serve)
                                                                         serve
                                                                         (error "unknown request
                 -- create-priority-queue" m))))))))
                                           dispatch))
                                     ((lambda (x) x)
                                        (let ((front (cons 'boe ())))
                                           (letrec ((content (lambda ()
                                                               (cdr front)))
                                                    (insert-after! (lambda (cell item)
                                                                     (let ((new-cell (cons item ())))
                                                                        (set-cdr! new-cell (cdr cell))
                                                                        (set-cdr! cell new-cell))))
                                                    (find-prev-cell (lambda (priority)
                                                                      (letrec ((find-iter (lambda (rest prev)
                                                                                            (if (null? rest)
                                                                                               prev
                                                                                               (if (> (get-priority (car rest)) priority)
                                                                                                  (find-iter (cdr rest) rest)
                                                                                                  prev)))))
                                                                         (find-iter (content) front))))
                                                    (empty? (lambda ()
                                                              (null? (content))))
                                                    (enqueue (lambda (priority element)
                                                               (insert-after! (find-prev-cell priority) (make-item priority element))
                                                               true))
                                                    (dequeue (lambda ()
                                                               (if (null? (content))
                                                                  false
                                                                  (let ((temp (car (content))))
                                                                     (set-cdr! front (cdr (content)))
                                                                     (get-element temp)))))
                                                    (serve (lambda ()
                                                             (if (null? (content))
                                                                false
                                                                (get-element (car (content))))))
                                                    (dispatch (lambda (m)
                                                                (if (eq? m 'empty?)
                                                                   empty?
                                                                   (if (eq? m 'enqueue)
                                                                      enqueue
                                                                      (if (eq? m 'dequeue)
                                                                         dequeue
                                                                         (if (eq? m 'serve)
                                                                            serve
                                                                            (error "unknown request
                 -- create-priority-queue" m))))))))
                                              dispatch))))))
         (pq (create-priority-queue)))
   ((pq 'enqueue) 66 'Patrick)
   ((pq 'enqueue) -106 'Octo)
   ((pq 'enqueue) 0 'Sandy)
   ((pq 'enqueue) 89 'Spongebob)
   (<change>
      ((pq 'dequeue))
      ((lambda (x) x) ((pq 'dequeue))))
   (equal? ((pq 'dequeue)) 'Patrick))