; Changes:
; * removed: 0
; * added: 4
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((false #f)
         (true #t)
         (create-stack (lambda (eq-fnct)
                         (<change>
                            (let ((content ()))
                               (letrec ((empty? (lambda ()
                                                  (null? content)))
                                        (push (lambda (element)
                                                (set! content (cons element content))
                                                #t))
                                        (pop (lambda ()
                                               (if (null? content)
                                                  #f
                                                  (let ((temp (car content)))
                                                     (set! content (cdr content))
                                                     temp))))
                                        (top (lambda ()
                                               (if (null? content) #f (car content))))
                                        (is-in (lambda (element)
                                                 (if (member element content) #t #f)))
                                        (dispatch (lambda (m)
                                                    (if (eq? m 'empty?)
                                                       empty?
                                                       (if (eq? m 'push)
                                                          push
                                                          (if (eq? m 'pop)
                                                             pop
                                                             (if (eq? m 'top)
                                                                top
                                                                (if (eq? m 'is-in)
                                                                   is-in
                                                                   (error "unknown request -- create-stack" m)))))))))
                                  dispatch))
                            ((lambda (x) x)
                               (let ((content ()))
                                  (<change>
                                     ()
                                     m)
                                  (letrec ((empty? (lambda ()
                                                     (null? content)))
                                           (push (lambda (element)
                                                   (<change>
                                                      ()
                                                      #t)
                                                   (set! content (cons element content))
                                                   #t))
                                           (pop (lambda ()
                                                  (if (null? content)
                                                     #f
                                                     (let ((temp (car content)))
                                                        (set! content (cdr content))
                                                        temp))))
                                           (top (lambda ()
                                                  (<change>
                                                     (if (null? content) #f (car content))
                                                     ((lambda (x) x) (if (null? content) #f (car content))))))
                                           (is-in (lambda (element)
                                                    (if (member element content) #t #f)))
                                           (dispatch (lambda (m)
                                                       (if (eq? m 'empty?)
                                                          empty?
                                                          (if (<change> (eq? m 'push) (not (eq? m 'push)))
                                                             push
                                                             (if (eq? m 'pop)
                                                                pop
                                                                (if (eq? m 'top)
                                                                   top
                                                                   (if (eq? m 'is-in)
                                                                      is-in
                                                                      (error "unknown request -- create-stack" m)))))))))
                                     dispatch)))))))
   (let ((stack (create-stack =)))
      (if ((stack 'empty?))
         (if (begin ((stack 'push) 13) (<change> () (display stack)) (not ((stack 'empty?))))
            (if ((stack 'is-in) 13)
               (if (= ((stack 'top)) 13)
                  (begin
                     ((stack 'push) 14)
                     (<change>
                        ()
                        'pop)
                     (= ((stack 'pop)) 14))
                  #f)
               #f)
            #f)
         #f)))