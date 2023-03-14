; Changes:
; * removed: 1
; * added: 3
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((false #f)
         (true #t)
         (create-stack (lambda (eq-fnct)
                         (let ((content ()))
                            (letrec ((empty? (lambda ()
                                               (<change>
                                                  ()
                                                  content)
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
                                                 (<change>
                                                    ()
                                                    m)
                                                 (if (eq? m 'empty?)
                                                    empty?
                                                    (if (eq? m 'push)
                                                       push
                                                       (if (eq? m 'pop)
                                                          (<change>
                                                             pop
                                                             (if (eq? m 'top)
                                                                top
                                                                (if (not (eq? m 'is-in))
                                                                   is-in
                                                                   (error "unknown request -- create-stack" m))))
                                                          (<change>
                                                             (if (eq? m 'top)
                                                                top
                                                                (if (eq? m 'is-in)
                                                                   is-in
                                                                   (error "unknown request -- create-stack" m)))
                                                             pop)))))))
                               dispatch)))))
   (<change>
      ()
      'push)
   (let ((stack (create-stack =)))
      (if ((stack 'empty?))
         (if (begin (<change> ((stack 'push) 13) ()) (not ((stack 'empty?))))
            (if ((stack 'is-in) 13)
               (if (= ((stack 'top)) 13)
                  (begin
                     ((stack 'push) 14)
                     (= ((stack 'pop)) 14))
                  #f)
               #f)
            #f)
         #f)))