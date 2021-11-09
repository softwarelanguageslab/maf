; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 2
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
                                                            (<change>
                                                               (set-cdr! rear (list (car element-list)))
                                                               (set! rear (cdr rear)))
                                                            (<change>
                                                               (set! rear (cdr rear))
                                                               (set-cdr! rear (list (car element-list))))))
                                                      (<change>
                                                         (enqueue (cdr element-list))
                                                         ((lambda (x) x) (enqueue (cdr element-list))))))))
                                     (dequeue (lambda ()
                                                (if (null? front)
                                                   (error "Can't front. The queue is empty.")
                                                   (let ((temp (car front)))
                                                      (<change>
                                                         ()
                                                         (display temp))
                                                      (set! front (cdr front))
                                                      (<change>
                                                         temp
                                                         ((lambda (x) x) temp))))))
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
      (if (= 1 (queue 'dequeue))
         (if (= 2 (queue 'dequeue))
            (<change>
               (if (= 3 (queue 'serve))
                  (if (= 3 (queue 'dequeue)) (queue 'empty?) #f)
                  #f)
               #f)
            (<change>
               #f
               (if (= 3 (queue 'serve))
                  (if (= 3 (queue 'dequeue)) (queue 'empty?) #f)
                  #f)))
         #f)
      #f))