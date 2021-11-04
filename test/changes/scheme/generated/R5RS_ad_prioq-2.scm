; Changes:
; * removed: 0
; * added: 1
; * swaps: 2
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((true #t)
         (false #f)
         (make-item (lambda (priority element)
                      (cons priority element)))
         (get-priority (lambda (item)
                         (car item)))
         (get-element (lambda (item)
                        (cdr item)))
         (create-priority-queue (lambda ()
                                  (let ((front (cons 'boe ())))
                                     (letrec ((content (lambda ()
                                                         (cdr front)))
                                              (insert-after! (lambda (cell item)
                                                               (let ((new-cell (cons item ())))
                                                                  (<change>
                                                                     ()
                                                                     cdr)
                                                                  (<change>
                                                                     (set-cdr! new-cell (cdr cell))
                                                                     (set-cdr! cell new-cell))
                                                                  (<change>
                                                                     (set-cdr! cell new-cell)
                                                                     (set-cdr! new-cell (cdr cell))))))
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
                                        dispatch))))
         (pq (create-priority-queue)))
   (<change>
      ((pq 'enqueue) 66 'Patrick)
      ((lambda (x) x) ((pq 'enqueue) 66 'Patrick)))
   ((pq 'enqueue) -106 'Octo)
   (<change>
      ((pq 'enqueue) 0 'Sandy)
      ((pq 'enqueue) 89 'Spongebob))
   (<change>
      ((pq 'enqueue) 89 'Spongebob)
      ((pq 'enqueue) 0 'Sandy))
   ((pq 'dequeue))
   (equal? ((pq 'dequeue)) 'Patrick))