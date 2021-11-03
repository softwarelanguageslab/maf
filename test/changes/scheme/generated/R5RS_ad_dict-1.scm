; Changes:
; * removed: 2
; * added: 1
; * swaps: 1
; * negated predicates: 2
; * swapped branches: 1
; * calls to id fun: 3
(letrec ((create-dictionary (lambda ()
                              (let ((content ()))
                                 (letrec ((empty? (lambda ()
                                                    (null? content)))
                                          (insert (lambda (key info)
                                                    (let ((temp (assoc key content)))
                                                       (if temp
                                                          (set-cdr! temp info)
                                                          (set! content (cons (cons key info) content))))
                                                    #t))
                                          (delete (lambda (key)
                                                    (letrec ((remove-iter (lambda (current prev)
                                                                            (if (<change> (null? current) (not (null? current)))
                                                                               #f
                                                                               (if (eq? key (caar current))
                                                                                  (begin
                                                                                     (if (null? prev)
                                                                                        (set! content (cdr content))
                                                                                        (set-cdr! prev (cdr current)))
                                                                                     #t)
                                                                                  (remove-iter (cdr current) current))))))
                                                       (remove-iter content ()))))
                                          (lookup (lambda (key)
                                                    (let ((temp (assoc key content)))
                                                       (if temp
                                                          (<change>
                                                             (cdr temp)
                                                             #f)
                                                          (<change>
                                                             #f
                                                             (cdr temp))))))
                                          (map (lambda (a-function)
                                                 (letrec ((map-iter (lambda (the-current result)
                                                                      (if (null? the-current)
                                                                         (reverse result)
                                                                         (map-iter (cdr the-current) (cons (a-function (caar the-current) (cdar the-current)) result))))))
                                                    (map-iter content ()))))
                                          (foreach (lambda (a-action)
                                                     (letrec ((foreach-iter (lambda (the-current)
                                                                              (if (null? the-current)
                                                                                 #t
                                                                                 (begin
                                                                                    (a-action (caar the-current) (cdar the-current))
                                                                                    (foreach-iter (cdr the-current)))))))
                                                        (<change>
                                                           (foreach-iter content)
                                                           ())
                                                        #t)))
                                          (display-dict (lambda ()
                                                          (foreach
                                                             (lambda (key info)
                                                                (<change>
                                                                   (display key)
                                                                   ((lambda (x) x) (display key)))
                                                                (display " ")
                                                                (display info)
                                                                (newline)))))
                                          (dispatch (lambda (msg args)
                                                      (if (eq? msg 'empty?)
                                                         (empty?)
                                                         (if (eq? msg 'insert)
                                                            (insert (car args) (cadr args))
                                                            (if (eq? msg 'delete)
                                                               (delete (car args))
                                                               (if (eq? msg 'lookup)
                                                                  (lookup (car args))
                                                                  (if (eq? msg 'map)
                                                                     (map (car args))
                                                                     (if (eq? msg 'foreach)
                                                                        (foreach (car args))
                                                                        (if (<change> (eq? msg 'display) (not (eq? msg 'display)))
                                                                           (display-dict)
                                                                           (error "unknown request -- create-dictionary" msg)))))))))))
                                    dispatch))))
         (nl->fr (create-dictionary)))
   (nl->fr 'insert (__toplevel_cons 'fiets (__toplevel_cons (__toplevel_cons 'bicyclette ()) ())))
   (nl->fr 'insert (__toplevel_cons 'auto (__toplevel_cons (__toplevel_cons 'voiture ()) ())))
   (<change>
      (nl->fr 'insert (__toplevel_cons 'huis (__toplevel_cons (__toplevel_cons 'maison ()) ())))
      ())
   (<change>
      (nl->fr 'insert (__toplevel_cons 'vrachtwagen (__toplevel_cons (__toplevel_cons 'camion ()) ())))
      ((lambda (x) x)
         (nl->fr 'insert (__toplevel_cons 'vrachtwagen (__toplevel_cons (__toplevel_cons 'camion ()) ())))))
   (<change>
      (nl->fr 'insert (__toplevel_cons 'tientonner (__toplevel_cons (__toplevel_cons 'camion ()) ())))
      (nl->fr 'lookup (__toplevel_cons 'fiets ())))
   (<change>
      (nl->fr 'lookup (__toplevel_cons 'fiets ()))
      (nl->fr 'insert (__toplevel_cons 'tientonner (__toplevel_cons (__toplevel_cons 'camion ()) ()))))
   (<change>
      (nl->fr 'display ())
      ((lambda (x) x) (nl->fr 'display ())))
   (<change>
      ()
      'lookup)
   (letrec ((fr->eng (create-dictionary)))
      (fr->eng 'insert (__toplevel_cons 'bicyclette (__toplevel_cons (__toplevel_cons 'bike ()) ())))
      (fr->eng 'insert (__toplevel_cons 'voiture (__toplevel_cons (__toplevel_cons 'car ()) ())))
      (fr->eng
         'insert
         (__toplevel_cons 'maison (__toplevel_cons (__toplevel_cons 'house (__toplevel_cons 'home ())) ())))
      (fr->eng 'insert (__toplevel_cons 'camion (__toplevel_cons (__toplevel_cons 'truck ()) ())))
      (fr->eng 'lookup (__toplevel_cons 'bicyclette ()))
      #t))