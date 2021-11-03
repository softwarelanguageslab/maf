; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 2
(letrec ((false #f)
         (true #t)
         (create-stack (lambda (eq-fnct)
                         (<change>
                            ()
                            car)
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
                                              (if (<change> (member element content) (not (member element content)))
                                                 #t
                                                 #f)))
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
   (let ((stack (create-stack =)))
      (if ((stack 'empty?))
         (if (<change> (begin ((stack 'push) 13) (not ((stack 'empty?)))) (not (begin ((stack 'push) 13) (not ((stack 'empty?))))))
            (if ((stack 'is-in) 13)
               (if (= ((stack 'top)) 13)
                  (begin
                     ((stack 'push) 14)
                     (= ((stack 'pop)) 14))
                  #f)
               #f)
            #f)
         #f)))