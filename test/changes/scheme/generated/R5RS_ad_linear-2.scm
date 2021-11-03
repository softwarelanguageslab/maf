; Changes:
; * removed: 0
; * added: 3
; * swaps: 3
; * negated predicates: 1
(letrec ((create-hash-table (lambda (size hash-fct)
                              (let ((content (make-vector size 0))
                                    (same? =))
                                 (letrec ((next-index (lambda (index)
                                                        (remainder (+ index 1) size)))
                                          (make-item (lambda (status key info)
                                                       (list status key info)))
                                          (get-status (lambda (item)
                                                        (car item)))
                                          (set-status! (lambda (item status)
                                                         (set-car! item status)))
                                          (get-key (lambda (item)
                                                     (cadr item)))
                                          (set-key! (lambda (item key)
                                                      (<change>
                                                         ()
                                                         set-car!)
                                                      (set-car! (cdr item) key)))
                                          (get-info (lambda (item)
                                                      (caddr item)))
                                          (set-info! (lambda (item info)
                                                       (<change>
                                                          ()
                                                          info)
                                                       (set-car! (cddr item) info)))
                                          (insert (lambda (key info)
                                                    (letrec ((rehash-iter (lambda (current)
                                                                            (let* ((item (vector-ref content current))
                                                                                   (status (get-status item)))
                                                                               (if (not (eq? status 'data))
                                                                                  (begin
                                                                                     (<change>
                                                                                        (set-status! item 'data)
                                                                                        (set-key! item key))
                                                                                     (<change>
                                                                                        (set-key! item key)
                                                                                        (set-status! item 'data))
                                                                                     (set-info! item info))
                                                                                  (if (same? key (get-key item))
                                                                                     (set-info! item info)
                                                                                     (rehash-iter (next-index current))))))))
                                                       (<change>
                                                          ()
                                                          hash-fct)
                                                       (rehash-iter (hash-fct key)))))
                                          (find-item (lambda (key)
                                                       (letrec ((rehash-iter (lambda (current)
                                                                               (let* ((item (vector-ref content current))
                                                                                      (status (get-status item)))
                                                                                  (if (eq? status 'data)
                                                                                     (if (same? key (get-key item))
                                                                                        item
                                                                                        (rehash-iter (next-index current)))
                                                                                     (if (eq? status 'empty)
                                                                                        #f
                                                                                        (rehash-iter (next-index current))))))))
                                                          (rehash-iter (hash-fct key)))))
                                          (retrieve (lambda (key)
                                                      (let ((temp (find-item key)))
                                                         (if temp (get-info temp) #f))))
                                          (delete (lambda (key)
                                                    (let ((temp (find-item key)))
                                                       (if temp
                                                          (begin
                                                             (set-status! temp 'deleted)
                                                             #t)
                                                          #f))))
                                          (display-table (lambda ()
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
                                                                 (iter 0)))))
                                          (dispatch (lambda (msg . args)
                                                      (if (eq? msg 'insert)
                                                         (insert (car args) (cadr args))
                                                         (if (eq? msg 'delete)
                                                            (delete (car args))
                                                            (if (eq? msg 'retrieve)
                                                               (retrieve (car args))
                                                               (if (eq? msg 'display)
                                                                  (display-table)
                                                                  (error "unknown request -- create-hash-table" msg))))))))
                                    (<change>
                                       (letrec ((__do_loop (lambda (index)
                                                             (if (negative? index)
                                                                'done
                                                                (begin
                                                                   (vector-set! content index (make-item 'empty () ()))
                                                                   (__do_loop (- index 1)))))))
                                          (__do_loop (- (vector-length content) 1)))
                                       dispatch)
                                    (<change>
                                       dispatch
                                       (letrec ((__do_loop (lambda (index)
                                                             (if (not (negative? index))
                                                                'done
                                                                (begin
                                                                   (vector-set! content index (make-item 'empty () ()))
                                                                   (__do_loop (- index 1)))))))
                                          (__do_loop (- (vector-length content) 1))))))))
         (table (create-hash-table 13 (lambda (key) (modulo key 13)))))
   (table 'insert 1 79)
   (<change>
      (table 'insert 4 69)
      (table 'insert 14 98))
   (<change>
      (table 'insert 14 98)
      (table 'insert 4 69))
   (table 'insert 7 72)
   (table 'insert 27 14)
   (table 'insert 11 50)
   (table 'display))