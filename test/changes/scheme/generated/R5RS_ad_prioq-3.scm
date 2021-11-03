; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 5
(letrec ((true #t)
         (false #f)
         (make-item (lambda (priority element)
                      (<change>
                         (cons priority element)
                         ((lambda (x) x) (cons priority element)))))
         (get-priority (lambda (item)
                         (car item)))
         (get-element (lambda (item)
                        (<change>
                           (cdr item)
                           ((lambda (x) x) (cdr item)))))
         (create-priority-queue (lambda ()
                                  (let ((front (cons 'boe ())))
                                     (letrec ((content (lambda ()
                                                         (cdr front)))
                                              (insert-after! (lambda (cell item)
                                                               (<change>
                                                                  (let ((new-cell (cons item ())))
                                                                     (set-cdr! new-cell (cdr cell))
                                                                     (set-cdr! cell new-cell))
                                                                  ((lambda (x) x)
                                                                     (let ((new-cell (cons item ())))
                                                                        (set-cdr! new-cell (cdr cell))
                                                                        (set-cdr! cell new-cell))))))
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
                                                         (<change>
                                                            (insert-after! (find-prev-cell priority) (make-item priority element))
                                                            true)
                                                         (<change>
                                                            true
                                                            (insert-after! (find-prev-cell priority) (make-item priority element)))))
                                              (dequeue (lambda ()
                                                         (if (null? (content))
                                                            false
                                                            (let ((temp (car (content))))
                                                               (set-cdr! front (cdr (content)))
                                                               (<change>
                                                                  ()
                                                                  (display content))
                                                               (<change>
                                                                  (get-element temp)
                                                                  ((lambda (x) x) (get-element temp)))))))
                                              (serve (lambda ()
                                                       (<change>
                                                          (if (null? (content))
                                                             false
                                                             (get-element (car (content))))
                                                          ((lambda (x) x) (if (null? (content)) false (get-element (car (content))))))))
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
                                        dispatch))))
         (pq (create-priority-queue)))
   ((pq 'enqueue) 66 'Patrick)
   ((pq 'enqueue) -106 'Octo)
   ((pq 'enqueue) 0 'Sandy)
   ((pq 'enqueue) 89 'Spongebob)
   ((pq 'dequeue))
   (equal? ((pq 'dequeue)) 'Patrick))