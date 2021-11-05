; Changes:
; * removed: 1
; * added: 1
; * swaps: 1
; * negated predicates: 2
; * swapped branches: 2
; * calls to id fun: 1
(letrec ((false #f)
         (true #t)
         (create-stack (lambda (eq-fnct)
                         (let ((content ()))
                            (letrec ((empty? (lambda ()
                                               (null? content)))
                                     (push (lambda (element)
                                             (set! content (cons element content))
                                             #t))
                                     (pop (lambda ()
                                            (<change>
                                               ()
                                               (display null?))
                                            (if (null? content)
                                               #f
                                               (let ((temp (car content)))
                                                  (<change>
                                                     (set! content (cdr content))
                                                     ())
                                                  temp))))
                                     (top (lambda ()
                                            (if (null? content) #f (car content))))
                                     (is-in (lambda (element)
                                              (if (member element content)
                                                 (<change>
                                                    #t
                                                    #f)
                                                 (<change>
                                                    #f
                                                    #t))))
                                     (dispatch (lambda (m)
                                                 (if (<change> (eq? m 'empty?) (not (eq? m 'empty?)))
                                                    empty?
                                                    (if (eq? m 'push)
                                                       push
                                                       (if (eq? m 'pop)
                                                          pop
                                                          (if (<change> (eq? m 'top) (not (eq? m 'top)))
                                                             top
                                                             (if (eq? m 'is-in)
                                                                is-in
                                                                (error "unknown request -- create-stack" m)))))))))
                               dispatch)))))
   (let ((stack (create-stack =)))
      (if ((stack 'empty?))
         (<change>
            (if (begin ((stack 'push) 13) (not ((stack 'empty?))))
               (if ((stack 'is-in) 13)
                  (if (= ((stack 'top)) 13)
                     (begin
                        ((stack 'push) 14)
                        (= ((stack 'pop)) 14))
                     #f)
                  #f)
               #f)
            #f)
         (<change>
            #f
            (if (begin (not ((stack 'empty?))) ((lambda (x) x) ((stack 'push) 13)))
               (if ((stack 'is-in) 13)
                  (if (= ((stack 'top)) 13)
                     (begin
                        ((stack 'push) 14)
                        (= ((stack 'pop)) 14))
                     #f)
                  #f)
               #f)))))