; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 4
(letrec ((false #f)
         (true #t)
         (create-stack (lambda (eq-fnct)
                         (let ((content ()))
                            (letrec ((empty? (lambda ()
                                               (<change>
                                                  ()
                                                  null?)
                                               (null? content)))
                                     (push (lambda (element)
                                             (<change>
                                                (set! content (cons element content))
                                                ((lambda (x) x) (set! content (cons element content))))
                                             (<change>
                                                #t
                                                ((lambda (x) x) #t))))
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
                               dispatch)))))
   (<change>
      (let ((stack (create-stack =)))
         (if ((stack 'empty?))
            (if (begin ((stack 'push) 13) (not ((stack 'empty?))))
               (if ((stack 'is-in) 13)
                  (if (= ((stack 'top)) 13)
                     (begin
                        ((stack 'push) 14)
                        (= ((stack 'pop)) 14))
                     #f)
                  #f)
               #f)
            #f))
      ((lambda (x) x)
         (let ((stack (create-stack =)))
            (if ((stack 'empty?))
               (if (begin ((stack 'push) 13) (not ((stack 'empty?))))
                  (if ((stack 'is-in) 13)
                     (if (<change> (= ((stack 'top)) 13) (not (= ((stack 'top)) 13)))
                        (begin
                           (<change>
                              ((stack 'push) 14)
                              ((lambda (x) x) ((stack 'push) 14)))
                           (= ((stack 'pop)) 14))
                        #f)
                     #f)
                  #f)
               #f)))))