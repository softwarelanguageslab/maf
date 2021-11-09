; Changes:
; * removed: 5
; * added: 4
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 1
; * calls to id fun: 4
(letrec ((create-hash-table (lambda (size hash-fct)
                              (let ((content (make-vector size 0))
                                    (same? =))
                                 (letrec ((next-index (lambda (index)
                                                        (<change>
                                                           (remainder (+ index 1) size)
                                                           ((lambda (x) x) (remainder (+ index 1) size)))))
                                          (make-item (lambda (status key info)
                                                       (list status key info)))
                                          (get-status (lambda (item)
                                                        (car item)))
                                          (set-status! (lambda (item status)
                                                         (set-car! item status)))
                                          (get-key (lambda (item)
                                                     (cadr item)))
                                          (set-key! (lambda (item key)
                                                      (set-car! (cdr item) key)))
                                          (get-info (lambda (item)
                                                      (caddr item)))
                                          (set-info! (lambda (item info)
                                                       (set-car! (cddr item) info)))
                                          (insert (lambda (key info)
                                                    (letrec ((rehash-iter (lambda (current)
                                                                            (let* ((item (vector-ref content current))
                                                                                   (status (get-status item)))
                                                                               (if (not (eq? status 'data))
                                                                                  (begin
                                                                                     (set-status! item 'data)
                                                                                     (set-key! item key)
                                                                                     (set-info! item info))
                                                                                  (if (same? key (get-key item))
                                                                                     (set-info! item info)
                                                                                     (rehash-iter (next-index current))))))))
                                                       (rehash-iter (hash-fct key)))))
                                          (find-item (lambda (key)
                                                       (<change>
                                                          ()
                                                          (rehash-iter (next-index current)))
                                                       (letrec ((rehash-iter (lambda (current)
                                                                               (let* ((item (vector-ref content current))
                                                                                      (status (get-status item)))
                                                                                  (if (<change> (eq? status 'data) (not (eq? status 'data)))
                                                                                     (if (same? key (get-key item))
                                                                                        item
                                                                                        (rehash-iter (next-index current)))
                                                                                     (if (eq? status 'empty)
                                                                                        #f
                                                                                        (rehash-iter (next-index current))))))))
                                                          (<change>
                                                             ()
                                                             hash-fct)
                                                          (rehash-iter (hash-fct key)))))
                                          (retrieve (lambda (key)
                                                      (let ((temp (find-item key)))
                                                         (if temp (get-info temp) #f))))
                                          (delete (lambda (key)
                                                    (<change>
                                                       (let ((temp (find-item key)))
                                                          (if temp
                                                             (begin
                                                                (set-status! temp 'deleted)
                                                                #t)
                                                             #f))
                                                       ((lambda (x) x)
                                                          (let ((temp (find-item key)))
                                                             (if temp
                                                                (begin
                                                                   (<change>
                                                                      (set-status! temp 'deleted)
                                                                      ())
                                                                   #t)
                                                                #f))))))
                                          (display-table (lambda ()
                                                           (<change>
                                                              (let ((stop (vector-length content)))
                                                                 (letrec ((iter (lambda (current)
                                                                                  (if (< current stop)
                                                                                     (begin
                                                                                        (display current)
                                                                                        (display "  ")
                                                                                        (display (vector-ref content current))
                                                                                        (newline)
                                                                                        (iter (+ current 1)))
                                                                                     #f))))
                                                                    (iter 0)))
                                                              ((lambda (x) x)
                                                                 (let ((stop (vector-length content)))
                                                                    (letrec ((iter (lambda (current)
                                                                                     (if (< current stop)
                                                                                        (<change>
                                                                                           (begin
                                                                                              (display current)
                                                                                              (display "  ")
                                                                                              (display (vector-ref content current))
                                                                                              (newline)
                                                                                              (iter (+ current 1)))
                                                                                           #f)
                                                                                        (<change>
                                                                                           #f
                                                                                           (begin
                                                                                              (display current)
                                                                                              newline
                                                                                              (iter (+ current 1))))))))
                                                                       (iter 0)))))))
                                          (dispatch (lambda (msg . args)
                                                      (<change>
                                                         ()
                                                         (eq? msg 'display))
                                                      (if (eq? msg 'insert)
                                                         (insert (car args) (cadr args))
                                                         (if (eq? msg 'delete)
                                                            (delete (car args))
                                                            (if (<change> (eq? msg 'retrieve) (not (eq? msg 'retrieve)))
                                                               (retrieve (car args))
                                                               (if (eq? msg 'display)
                                                                  (display-table)
                                                                  (error "unknown request -- create-hash-table" msg))))))))
                                    (letrec ((__do_loop (lambda (index)
                                                          (if (negative? index)
                                                             'done
                                                             (begin
                                                                (<change>
                                                                   (vector-set! content index (make-item 'empty () ()))
                                                                   ())
                                                                (__do_loop (- index 1)))))))
                                       (__do_loop (- (vector-length content) 1)))
                                    dispatch))))
         (table (create-hash-table 13 (lambda (key) (modulo key 13)))))
   (table 'insert 1 79)
   (table 'insert 4 69)
   (table 'insert 14 98)
   (<change>
      (table 'insert 7 72)
      ((lambda (x) x) (table 'insert 7 72)))
   (table 'insert 27 14)
   (table 'insert 11 50)
   (table 'display))