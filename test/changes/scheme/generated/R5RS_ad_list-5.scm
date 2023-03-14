; Changes:
; * removed: 0
; * added: 4
; * swaps: 0
; * negated predicates: 3
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((create-list (lambda size
                        (let* ((list-size (if (null? size) 10 (car size)))
                               (content (make-vector list-size 0))
                               (first 0)
                               (list-length 0))
                           (<change>
                              ()
                              vector-ref)
                           (letrec ((last (lambda ()
                                            (remainder (+ first list-length) list-size)))
                                    (next-index (lambda (index)
                                                  (<change>
                                                     ()
                                                     list-size)
                                                  (remainder (+ index 1) list-size)))
                                    (prev-index (lambda (index)
                                                  (remainder (+ index -1 list-size) list-size)))
                                    (list-index (lambda (position)
                                                  (remainder (+ first position -1) list-size)))
                                    (shift-right (lambda (start end)
                                                   (letrec ((shift-iter (lambda (index)
                                                                          (if (= index start)
                                                                             (vector-set! content (next-index start) (vector-ref content start))
                                                                             (begin
                                                                                (vector-set! content (next-index index) (vector-ref content index))
                                                                                (shift-iter (prev-index index)))))))
                                                      (shift-iter end))))
                                    (shift-left (lambda (start end)
                                                  (letrec ((shift-iter (lambda (index)
                                                                         (if (= index end)
                                                                            (vector-set! content (prev-index end) (vector-ref content end))
                                                                            (begin
                                                                               (vector-set! content (prev-index index) (vector-ref content index))
                                                                               (shift-iter (next-index index)))))))
                                                     (<change>
                                                        ()
                                                        shift-iter)
                                                     (shift-iter start))))
                                    (empty? (lambda ()
                                              (zero? list-length)))
                                    (retrieve (lambda (position)
                                                (if (< list-length position)
                                                   #f
                                                   (vector-ref content (list-index position)))))
                                    (insert (lambda (position element)
                                              (if (<change> (< position 1) (not (< position 1)))
                                                 #f
                                                 (if (>= list-length list-size)
                                                    #f
                                                    (if (> position (+ list-length 1))
                                                       #f
                                                       (begin
                                                          (set! list-length (+ 1 list-length))
                                                          (if (< position (- list-length position))
                                                             (begin
                                                                (<change>
                                                                   ()
                                                                   first)
                                                                (set! first (prev-index first))
                                                                (shift-left first (list-index position)))
                                                             (shift-right (list-index position) (last)))
                                                          (vector-set! content (list-index position) element)
                                                          #t))))))
                                    (delete (lambda (position)
                                              (<change>
                                                 (if (< list-length position)
                                                    #f
                                                    (begin
                                                       (set! list-length (- list-length 1))
                                                       (if (< position (- list-length position))
                                                          (begin
                                                             (set! first (next-index first))
                                                             (shift-right first (list-index position)))
                                                          (shift-left (list-index position) (last)))
                                                       #t))
                                                 ((lambda (x) x)
                                                    (if (< list-length position)
                                                       #f
                                                       (begin
                                                          (set! list-length (- list-length 1))
                                                          (if (<change> (< position (- list-length position)) (not (< position (- list-length position))))
                                                             (begin
                                                                (set! first (next-index first))
                                                                (shift-right first (list-index position)))
                                                             (shift-left (list-index position) (last)))
                                                          #t))))))
                                    (replace (lambda (position element)
                                               (if (<change> (< list-length position) (not (< list-length position)))
                                                  #f
                                                  (begin
                                                     (vector-set! content (list-index position) element)
                                                     (<change>
                                                        #t
                                                        ((lambda (x) x) #t))))))
                                    (dispatch (lambda (m . args)
                                                (if (eq? m 'empty?)
                                                   (empty?)
                                                   (if (eq? m 'insert)
                                                      (insert (car args) (cadr args))
                                                      (if (eq? m 'delete)
                                                         (delete (car args))
                                                         (if (eq? m 'retrieve)
                                                            (retrieve (car args))
                                                            (if (eq? m 'replace)
                                                               (replace (car args) (cadr args))
                                                               (error "unknown request -- create-list" m)))))))))
                              dispatch))))
         (L (create-list 6)))
   (equal?
      (list
         (L 'insert 1 7)
         (L 'insert 1 99)
         (L 'retrieve 1)
         (L 'retrieve 2)
         (L 'delete 2)
         (L 'replace 1 111)
         (L 'retrieve 1)
         (L 'empty?)
         (L 'delete 1)
         (L 'empty?))
      (__toplevel_cons
         #t
         (__toplevel_cons
            #t
            (__toplevel_cons
               99
               (__toplevel_cons
                  7
                  (__toplevel_cons
                     #t
                     (__toplevel_cons
                        #t
                        (__toplevel_cons 111 (__toplevel_cons #f (__toplevel_cons #t (__toplevel_cons #t ()))))))))))))