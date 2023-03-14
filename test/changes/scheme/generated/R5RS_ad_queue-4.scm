; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((create-queue (lambda ()
                         (let ((front ())
                               (rear ()))
                            (letrec ((empty? (lambda ()
                                               (null? front)))
                                     (enqueue (lambda (element-list)
                                                (if (null? element-list)
                                                   #t
                                                   (begin
                                                      (if (null? front)
                                                         (begin
                                                            (set! front (list (car element-list)))
                                                            (set! rear front))
                                                         (begin
                                                            (set-cdr! rear (list (car element-list)))
                                                            (set! rear (cdr rear))))
                                                      (enqueue (cdr element-list))))))
                                     (dequeue (lambda ()
                                                (if (null? front)
                                                   (error "Can't front. The queue is empty.")
                                                   (let ((temp (car front)))
                                                      (set! front (cdr front))
                                                      temp))))
                                     (serve (lambda ()
                                              (if (null? front)
                                                 (error "Can't serve. The queue is empty.")
                                                 (car front))))
                                     (dispatch (lambda (msg . args)
                                                 (if (eq? msg 'empty?)
                                                    (empty?)
                                                    (if (eq? msg 'enqueue)
                                                       (enqueue args)
                                                       (if (eq? msg 'dequeue)
                                                          (dequeue)
                                                          (if (eq? msg 'serve)
                                                             (serve)
                                                             (error "unknown request -- create-queue" msg))))))))
                               dispatch))))
         (queue (create-queue)))
   (<change>
      (queue 'enqueue 1 2 3)
      (if (not (queue 'empty?))
         (if (= 1 (queue 'dequeue))
            (if (= 2 (queue 'dequeue))
               (if (= 3 (queue 'serve))
                  (if (= 3 (queue 'dequeue)) (queue 'empty?) #f)
                  #f)
               #f)
            #f)
         #f))
   (<change>
      (if (not (queue 'empty?))
         (if (= 1 (queue 'dequeue))
            (if (= 2 (queue 'dequeue))
               (if (= 3 (queue 'serve))
                  (if (= 3 (queue 'dequeue)) (queue 'empty?) #f)
                  #f)
               #f)
            #f)
         #f)
      (queue 'enqueue 1 2 3)))