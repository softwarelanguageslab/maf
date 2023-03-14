(define BufferSize (int-top))
(define NumProducers (int-top))
(define NumConsumers (int-top))
(define NumItemsPerProducer (int-top))

(define ConsCost 40)
(define ProdCost 40)
(define AdjustedBufferSize (- BufferSize NumProducers))

(define (for-each f l)
  (if (null? l)
      #t
      (if (pair? l)
          (begin (f (car l)) (for-each f (cdr l)))
          (error "Cannot for-each over a non-list"))))

(define (spawn-producers manager)
  (letrec ((loop (lambda (i)
                   (if (= i NumProducers)
                       '()
                       (let ((p (create producer i manager 0 0)))
                         (send p produce-data)
                         (cons p (loop (+ i 1))))))))
    (loop 0)))

(define (spawn-consumers manager)
  (letrec ((loop (lambda (i)
                   (if (= i NumConsumers)
                       '()
                       (let ((c (create consumer i manager 0)))
                         (cons c (loop (+ i 1))))))))
    (loop 0)))

(define manager
  (actor "manager" (producers consumers available-producers available-consumers pending-data terminated-producers)
           (start ()
                  (let ((producers (spawn-producers a/self))
                        (consumers (spawn-consumers a/self)))
                    (become manager producers consumers '() consumers pending-data terminated-producers)))
           (data-item (producer data)
                      (if (null? available-consumers)
                          (if (> (length pending-data) AdjustedBufferSize)
                              (become manager producers consumers (cons producer available-producers) available-consumers (cons data pending-data) terminated-producers)
                              (begin
                                (send producer produce-data)
                                (become manager producers consumers available-producers available-consumers (cons data pending-data) terminated-producers)))
                          (let ((available-producers
                                 (if (> (length pending-data) AdjustedBufferSize)
                                     (cons producer available-producers)
                                     (begin
                                       (send producer produce-data)
                                       available-producers))))
                            (send (car available-consumers) data-item data)
                            (become manager producers consumers
                                      available-producers (cdr available-consumers)
                                      (cons data pending-data) terminated-producers))))
           (consumer-available (consumer)
                               (if (null? pending-data)
                                   (if (and (= terminated-producers NumProducers) (= (+ 1 (length available-consumers)) NumConsumers))
                                       (begin
                                         (for-each (lambda (c) (send c exit)) consumers)
                                         (terminate))
                                       (become manager producers consumers available-producers (cons consumer available-consumers) pending-data terminated-producers))
                                   (begin
                                     (send consumer data-item (car pending-data))
                                     (if (not (null? available-producers))
                                         (begin
                                           (send (car available-producers) produce-data)
                                           (become manager producers consumers (cdr available-producers) available-consumers (cdr pending-data) terminated-producers))
                                         (become manager producers consumers available-producers available-consumers (cdr pending-data) terminated-producers)))))
           (exit () (if (and (= (+ 1 terminated-producers) NumProducers) (= (length available-consumers) NumConsumers))
                        (begin
                          (display (display "available consumers: ~a~n " (length available-consumers)))
                          (for-each (lambda (c) (send c exit)) consumers)
                          (terminate))
                        (become manager producers consumers available-producers available-consumers pending-data (+ 1 terminated-producers))))))

(define (process-item item cost) (random 10)) ; not modeled as in Savina, but doesn't change anything for static analysis

(define producer (actor "producer" (id manager items-produced prod-item)
                          (produce-data ()
                                        (if (= items-produced NumItemsPerProducer)
                                            (begin
                                              (send manager exit)
                                              (terminate))
                                            (begin
                                              (let ((prod-item2 (process-item prod-item ProdCost)))
                                                (send manager data-item a/self prod-item)
                                                (become producer id manager (+ items-produced 1) prod-item2)))))))
(define consumer (actor "consumer" (id manager cons-item)
                          (data-item (data)
                                     (let ((cons-item2 (process-item (+ cons-item data) ConsCost)))
                                       (send manager consumer-available a/self)
                                       (become consumer id manager cons-item2)))
                          (exit () (terminate))))
(define m (create manager '() '() '() '() '() 0))
(send m start)

