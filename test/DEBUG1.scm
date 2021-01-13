(define (append . lsts)
  (define (app lsts)
    (cond ((null? lsts) '())
          ((null? (cdr lsts)) (car lsts)) ; Structure sharing.
          (else (let loop ((first (car lsts))
                           (rest (app (cdr lsts))))
                  (if (null? first)
                      rest
                      (cons (car first)
                            (loop (cdr first)
                                  rest)))))))
  (app lsts))

(display (append '(a b c) '(d e f) 3))