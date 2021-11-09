; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
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
                                                (if (<change> (null? front) (not (null? front)))
                                                   (error "Can't front. The queue is empty.")
                                                   (let ((temp (car front)))
                                                      (set! front (cdr front))
                                                      (<change>
                                                         ()
                                                         (display front))
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
   (queue 'enqueue 1 2 3)
   (if (not (queue 'empty?))
      (<change>
         (if (= 1 (queue 'dequeue))
            (if (= 2 (queue 'dequeue))
               (if (= 3 (queue 'serve))
                  (if (= 3 (queue 'dequeue)) (queue 'empty?) #f)
                  #f)
               #f)
            #f)
         #f)
      (<change>
         #f
         (if (= 1 (queue 'dequeue))
            (if (= 2 (queue 'dequeue))
               (if (= 3 (queue 'serve))
                  (if (= 3 (queue 'dequeue)) (queue 'empty?) #f)
                  #f)
               #f)
            #f))))