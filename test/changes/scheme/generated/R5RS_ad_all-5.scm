; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((bubble-sort (lambda (vector)
                        (letrec ((swap (lambda (vector index1 index2)
                                         (let ((temp (vector-ref vector index1)))
                                            (vector-set! vector index1 (vector-ref vector index2))
                                            (vector-set! vector index2 temp))))
                                 (bubble (lambda (index)
                                           (letrec ((bubble-iter (lambda (index1 changed)
                                                                   (if (<= index1 index)
                                                                      (begin
                                                                         (<change>
                                                                            ()
                                                                            (display index1))
                                                                         (if (> (vector-ref vector index1) (vector-ref vector (+ index1 1)))
                                                                            (begin
                                                                               (swap vector index1 (+ index1 1))
                                                                               (set! changed #t))
                                                                            #f)
                                                                         (bubble-iter (+ index1 1) changed))
                                                                      changed))))
                                              (bubble-iter 0 #f))))
                                 (bubble-sort-iter (lambda (index)
                                                     (if (<change> (>= index 0) (not (>= index 0)))
                                                        (if (bubble index)
                                                           (bubble-sort-iter (- index 1))
                                                           #f)
                                                        #f))))
                           (bubble-sort-iter (- (vector-length vector) 2)))))
         (vect (vector 9 5 1 7 8 9 4 6 2 3)))
   (<change>
      (bubble-sort vect)
      (equal? vect (vector 1 2 3 4 5 6 7 8 9 9)))
   (<change>
      (equal? vect (vector 1 2 3 4 5 6 7 8 9 9))
      (bubble-sort vect))
   (letrec ((selection-sort (lambda (vector)
                              (letrec ((swap (lambda (vector index1 index2)
                                               (let ((temp (vector-ref vector index1)))
                                                  (vector-set! vector index1 (vector-ref vector index2))
                                                  (vector-set! vector index2 temp))))
                                       (pos-of-min (lambda (vector low high)
                                                     (letrec ((min-iter (lambda (index pos-of-min-so-far)
                                                                          (if (<= index high)
                                                                             (if (< (vector-ref vector index) (vector-ref vector pos-of-min-so-far))
                                                                                (min-iter (+ index 1) index)
                                                                                (min-iter (+ index 1) pos-of-min-so-far))
                                                                             pos-of-min-so-far))))
                                                        (min-iter (+ low 1) low)))))
                                 (let ((high (- (vector-length vector) 1)))
                                    (letrec ((selection-sort-iter (lambda (index)
                                                                    (if (< index high)
                                                                       (begin
                                                                          (swap vector index (pos-of-min vector index high))
                                                                          (selection-sort-iter (+ index 1)))
                                                                       #f))))
                                       (selection-sort-iter 0))))))
            (vect2 (vector 5 7 0 9 6 4 3 8 2 1)))
      (selection-sort vect2)
      (equal? vect2 (vector 0 1 2 3 4 5 6 7 8 9))
      (letrec ((result ())
               (display2 (lambda (item)
                           (set! result (cons item result))))
               (newline2 (lambda ()
                           (set! result (cons 'newline result))))
               (make-row (lambda (key name age wage)
                           (vector key name age wage)))
               (key-ref (lambda (row)
                          (vector-ref row 0)))
               (name-ref (lambda (row)
                           (vector-ref row 1)))
               (age-ref (lambda (row)
                          (vector-ref row 2)))
               (wage-ref (lambda (row)
                           (vector-ref row 3)))
               (key-set! (lambda (row value)
                           (vector-set! row 0 value)))
               (name-set! (lambda (row value)
                            (vector-set! row 1 value)))
               (age-set! (lambda (row value)
                           (vector-set! row 2 value)))
               (wage-set! (lambda (row value)
                            (vector-set! row 3 value)))
               (show-row (lambda (row)
                           (display2 "[Sleutel:")
                           (display2 (key-ref row))
                           (display2 "]")
                           (display2 "[Naam:")
                           (display2 (name-ref row))
                           (display2 "]")
                           (display2 "[Leeftijd:")
                           (display2 (age-ref row))
                           (display2 "]")
                           (display2 "[Salaris:")
                           (display2 (wage-ref row))
                           (display2 "]")))
               (make-table (lambda (rows)
                             (make-vector rows 0)))
               (table-size (lambda (table)
                             (vector-length table)))
               (row-ref (lambda (table pos)
                          (if (< pos (table-size table))
                             (vector-ref table pos)
                             #f)))
               (row-set! (lambda (table pos row)
                           (if (< pos (table-size table))
                              (vector-set! table pos row)
                              #f)))
               (show-table (lambda (table)
                             (letrec ((iter (lambda (index)
                                              (if (= index (table-size table))
                                                 (newline2)
                                                 (begin
                                                    (show-row (row-ref table index))
                                                    (newline2)
                                                    (iter (+ index 1)))))))
                                (iter 0))))
               (table (make-table 10)))
         (row-set! table 0 (make-row 8 'Bernard 45 120000))
         (row-set! table 1 (make-row 3 'Dirk 26 93000))
         (row-set! table 2 (make-row 6 'George 48 130000))
         (row-set! table 3 (make-row 6 'Greet 27 75000))
         (row-set! table 4 (make-row 1 'Kaat 18 69000))
         (row-set! table 5 (make-row 5 'Mauranne 21 69000))
         (row-set! table 6 (make-row 4 'Peter 33 80000))
         (row-set! table 7 (make-row 2 'Piet 25 96000))
         (row-set! table 8 (make-row 9 'Tom 26 96000))
         (row-set! table 9 (make-row 6 'Veronique 36 115000))
         (letrec ((expected-result (__toplevel_cons
                                     'newline
                                     (__toplevel_cons
                                        'newline
                                        (__toplevel_cons
                                           "]"
                                           (__toplevel_cons
                                              115000
                                              (__toplevel_cons
                                                 "[Salaris:"
                                                 (__toplevel_cons
                                                    "]"
                                                    (__toplevel_cons
                                                       36
                                                       (__toplevel_cons
                                                          "[Leeftijd:"
                                                          (__toplevel_cons
                                                             "]"
                                                             (__toplevel_cons
                                                                'Veronique
                                                                (__toplevel_cons
                                                                   "[Naam:"
                                                                   (__toplevel_cons
                                                                      "]"
                                                                      (__toplevel_cons
                                                                         6
                                                                         (__toplevel_cons
                                                                            "[Sleutel:"
                                                                            (__toplevel_cons
                                                                               'newline
                                                                               (__toplevel_cons
                                                                                  "]"
                                                                                  (__toplevel_cons
                                                                                     96000
                                                                                     (__toplevel_cons
                                                                                        "[Salaris:"
                                                                                        (__toplevel_cons
                                                                                           "]"
                                                                                           (__toplevel_cons
                                                                                              26
                                                                                              (__toplevel_cons
                                                                                                 "[Leeftijd:"
                                                                                                 (__toplevel_cons
                                                                                                    "]"
                                                                                                    (__toplevel_cons
                                                                                                       'Tom
                                                                                                       (__toplevel_cons
                                                                                                          "[Naam:"
                                                                                                          (__toplevel_cons
                                                                                                             "]"
                                                                                                             (__toplevel_cons
                                                                                                                9
                                                                                                                (__toplevel_cons
                                                                                                                   "[Sleutel:"
                                                                                                                   (__toplevel_cons
                                                                                                                      'newline
                                                                                                                      (__toplevel_cons
                                                                                                                         "]"
                                                                                                                         (__toplevel_cons
                                                                                                                            96000
                                                                                                                            (__toplevel_cons
                                                                                                                               "[Salaris:"
                                                                                                                               (__toplevel_cons
                                                                                                                                  "]"
                                                                                                                                  (__toplevel_cons
                                                                                                                                     25
                                                                                                                                     (__toplevel_cons
                                                                                                                                        "[Leeftijd:"
                                                                                                                                        (__toplevel_cons
                                                                                                                                           "]"
                                                                                                                                           (__toplevel_cons
                                                                                                                                              'Piet
                                                                                                                                              (__toplevel_cons
                                                                                                                                                 "[Naam:"
                                                                                                                                                 (__toplevel_cons
                                                                                                                                                    "]"
                                                                                                                                                    (__toplevel_cons
                                                                                                                                                       2
                                                                                                                                                       (__toplevel_cons
                                                                                                                                                          "[Sleutel:"
                                                                                                                                                          (__toplevel_cons
                                                                                                                                                             'newline
                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                "]"
                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                   80000
                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                      "[Salaris:"
                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                         "]"
                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                            33
                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                               "[Leeftijd:"
                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                  "]"
                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                     'Peter
                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                        "[Naam:"
                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                           "]"
                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                              4
                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                 "[Sleutel:"
                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                    'newline
                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                       "]"
                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                          69000
                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                             "[Salaris:"
                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                "]"
                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                   21
                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                      "[Leeftijd:"
                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                         "]"
                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                            'Mauranne
                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                               "[Naam:"
                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                  "]"
                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                     5
                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                        "[Sleutel:"
                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                           'newline
                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                              "]"
                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                 69000
                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                    "[Salaris:"
                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                       "]"
                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                          18
                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                             "[Leeftijd:"
                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                "]"
                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                   'Kaat
                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                      "[Naam:"
                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                         "]"
                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                            1
                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                               "[Sleutel:"
                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                  'newline
                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                     "]"
                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                        75000
                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                           "[Salaris:"
                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                              "]"
                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                 27
                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                    "[Leeftijd:"
                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                       "]"
                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                          'Greet
                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                             "[Naam:"
                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                "]"
                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                   6
                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                      "[Sleutel:"
                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                         'newline
                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                            "]"
                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                               130000
                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                  "[Salaris:"
                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                     "]"
                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                        48
                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                           "[Leeftijd:"
                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                              "]"
                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                 'George
                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                    "[Naam:"
                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                       "]"
                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                          6
                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                             "[Sleutel:"
                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                'newline
                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                   "]"
                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                      93000
                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                         "[Salaris:"
                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                            "]"
                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                               26
                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                  "[Leeftijd:"
                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                     "]"
                                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                        'Dirk
                                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                           "[Naam:"
                                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                              "]"
                                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                 3
                                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                    "[Sleutel:"
                                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                       'newline
                                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                          "]"
                                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                             120000
                                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                "[Salaris:"
                                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                   "]"
                                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                      45
                                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                         "[Leeftijd:"
                                                                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                            "]"
                                                                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                               'Bernard
                                                                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                  "[Naam:"
                                                                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons "]" (__toplevel_cons 8 (__toplevel_cons "[Sleutel:" ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
            (show-table table)
            (equal? expected-result result)
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
                                                                                        (if (null? current)
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
                                                                   (if temp (cdr temp) #f))))
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
                                                                    (foreach-iter content)
                                                                    #t)))
                                                      (display-dict (lambda ()
                                                                      (foreach (lambda (key info) (display key) (display " ") (display info) (newline)))))
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
                                                                                    (if (eq? msg 'display)
                                                                                       (display-dict)
                                                                                       (error "unknown request -- create-dictionary" msg)))))))))))
                                                dispatch))))
                     (nl->fr (create-dictionary)))
               (nl->fr 'insert (__toplevel_cons 'fiets (__toplevel_cons (__toplevel_cons 'bicyclette ()) ())))
               (nl->fr 'insert (__toplevel_cons 'auto (__toplevel_cons (__toplevel_cons 'voiture ()) ())))
               (nl->fr 'insert (__toplevel_cons 'huis (__toplevel_cons (__toplevel_cons 'maison ()) ())))
               (nl->fr 'insert (__toplevel_cons 'vrachtwagen (__toplevel_cons (__toplevel_cons 'camion ()) ())))
               (nl->fr 'insert (__toplevel_cons 'tientonner (__toplevel_cons (__toplevel_cons 'camion ()) ())))
               (nl->fr 'lookup (__toplevel_cons 'fiets ()))
               (nl->fr 'display ())
               (letrec ((fr->eng (create-dictionary)))
                  (fr->eng 'insert (__toplevel_cons 'bicyclette (__toplevel_cons (__toplevel_cons 'bike ()) ())))
                  (fr->eng 'insert (__toplevel_cons 'voiture (__toplevel_cons (__toplevel_cons 'car ()) ())))
                  (fr->eng
                     'insert
                     (__toplevel_cons 'maison (__toplevel_cons (__toplevel_cons 'house (__toplevel_cons 'home ())) ())))
                  (fr->eng 'insert (__toplevel_cons 'camion (__toplevel_cons (__toplevel_cons 'truck ()) ())))
                  (fr->eng 'lookup (__toplevel_cons 'bicyclette ()))
                  #t
                  (letrec ((my-++ (lambda (n)
                                    (+ n 1)))
                           (my--- (lambda (n)
                                    (- n 1)))
                           (false #f)
                           (true #t)
                           (nil ())
                           (key (lambda (x)
                                  x))
                           (make-heap (lambda (a-vector nr-of-elements)
                                        (letrec ((iter (lambda (index)
                                                         (if (> index 0)
                                                            (begin
                                                               (sift-down a-vector index nr-of-elements)
                                                               (iter (my--- index)))
                                                            #f))))
                                           (iter (quotient nr-of-elements 2)))))
                           (sift-down (lambda (heap from to)
                                        (letrec ((smallest-child (lambda (parent)
                                                                   (let* ((child1 (* 2 parent))
                                                                          (child2 (my-++ child1)))
                                                                      (if (> child1 to)
                                                                         false
                                                                         (if (> child2 to)
                                                                            child1
                                                                            (if (< (key (vector-ref heap child1)) (key (vector-ref heap child2)))
                                                                               child1
                                                                               child2))))))
                                                 (iter (lambda (parent)
                                                         (let ((child (smallest-child parent)))
                                                            (if child
                                                               (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                                                  (begin
                                                                     (swap heap child parent)
                                                                     (iter child))
                                                                  #f)
                                                               #f)))))
                                           (iter from))))
                           (swap (lambda (a-vector i1 i2)
                                   (let ((temp (vector-ref a-vector i1)))
                                      (vector-set! a-vector i1 (vector-ref a-vector i2))
                                      (vector-set! a-vector i2 temp))))
                           (sift-up (lambda (heap from)
                                      (letrec ((iter (lambda (child)
                                                       (let ((parent (quotient child 2)))
                                                          (if (> parent 0)
                                                             (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                                                                (begin
                                                                   (swap heap child parent)
                                                                   (iter parent))
                                                                #f)
                                                             #f)))))
                                         (iter from))))
                           (create-heap (lambda (size)
                                          (cons 0 (make-vector (my-++ size)))))
                           (is-empty? (lambda (heap)
                                        (eq? (car heap) 0)))
                           (insert (lambda (heap item)
                                     (let* ((content (cdr heap))
                                            (new-nr-of-elements (my-++ (car heap)))
                                            (size (my--- (vector-length content))))
                                        (display "insert    ")
                                        (if (> new-nr-of-elements size)
                                           false
                                           (begin
                                              (vector-set! content new-nr-of-elements item)
                                              (sift-up content new-nr-of-elements)
                                              (set-car! heap new-nr-of-elements)))
                                        (display heap)
                                        (newline))))
                           (v (vector 'lol 5 8 1 3 9 10 2 0)))
                     (make-heap v 8)
                     (equal? v (vector 'lol 0 3 1 5 9 10 2 8))
                     (letrec ((quick-sort (lambda (a-list)
                                            (letrec ((rearrange (lambda (pivot some-list)
                                                                  (letrec ((rearrange-iter (lambda (rest result)
                                                                                             (if (null? rest)
                                                                                                result
                                                                                                (if (<= (car rest) pivot)
                                                                                                   (rearrange-iter (cdr rest) (cons (cons (car rest) (car result)) (cdr result)))
                                                                                                   (rearrange-iter (cdr rest) (cons (car result) (cons (car rest) (cdr result)))))))))
                                                                     (rearrange-iter some-list (cons () ()))))))
                                               (if (<= (length a-list) 1)
                                                  a-list
                                                  (let* ((pivot (car a-list))
                                                         (sub-lists (rearrange pivot (cdr a-list))))
                                                     (append (quick-sort (car sub-lists)) (append (list pivot) (quick-sort (cdr sub-lists))))))))))
                        (equal?
                           (quick-sort
                              (__toplevel_cons
                                 9
                                 (__toplevel_cons
                                    8
                                    (__toplevel_cons
                                       7
                                       (__toplevel_cons
                                          6
                                          (__toplevel_cons
                                             5
                                             (__toplevel_cons
                                                4
                                                (__toplevel_cons
                                                   3
                                                   (__toplevel_cons 2 (__toplevel_cons 1 (__toplevel_cons 0 (__toplevel_cons 9 ()))))))))))))
                           (__toplevel_cons
                              0
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons
                                    2
                                    (__toplevel_cons
                                       3
                                       (__toplevel_cons
                                          4
                                          (__toplevel_cons
                                             5
                                             (__toplevel_cons
                                                6
                                                (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 (__toplevel_cons 9 ()))))))))))))
                        (letrec ((insertion-sort (lambda (vector)
                                                   (let ((high (- (vector-length vector) 1)))
                                                      (letrec ((shift-left (lambda (vector index)
                                                                             (vector-set! vector (- index 1) (vector-ref vector index))))
                                                               (insert-sort-iter (lambda (index1)
                                                                                   (letrec ((insert (lambda (index1)
                                                                                                      (let ((insert-value (vector-ref vector (- index1 1))))
                                                                                                         (letrec ((insert-iter (lambda (index2)
                                                                                                                                 (if (if (<= index2 high) (< (vector-ref vector index2) insert-value) #f)
                                                                                                                                    (begin
                                                                                                                                       (shift-left vector index2)
                                                                                                                                       (insert-iter (+ index2 1)))
                                                                                                                                    (vector-set! vector (- index2 1) insert-value)))))
                                                                                                            (insert-iter index1))))))
                                                                                      (if (> index1 0)
                                                                                         (begin
                                                                                            (insert index1)
                                                                                            (insert-sort-iter (- index1 1)))
                                                                                         #f)))))
                                                         (insert-sort-iter high)))))
                                 (vect3 (vector 5 2 7 1 0 9 8 6 3 4)))
                           (insertion-sort vect3)
                           (equal? vect3 (vector 0 1 2 3 4 5 6 7 8 9))
                           (letrec ((make-item (lambda (priority element)
                                                 (cons priority element)))
                                    (get-priority (lambda (item)
                                                    (car item)))
                                    (get-element (lambda (item)
                                                   (cdr item)))
                                    (create-priority-queue (lambda ()
                                                             (let ((front (cons 'boe ())))
                                                                (letrec ((content (lambda ()
                                                                                    (cdr front)))
                                                                         (insert-after! (lambda (cell item)
                                                                                          (let ((new-cell (cons item ())))
                                                                                             (set-cdr! new-cell (cdr cell))
                                                                                             (set-cdr! cell new-cell))))
                                                                         (find-prev-cell (lambda (priority)
                                                                                           (letrec ((find-iter (lambda (rest prev)
                                                                                                                 (if (null? rest)
                                                                                                                    prev
                                                                                                                    (if (> (get-priority (car rest)) priority)
                                                                                                                       (find-iter (cdr rest) rest)
                                                                                                                       prev)))))
                                                                                              (find-iter (content) front))))
                                                                         (empty? (lambda ()
                                                                                   (null? (content))))
                                                                         (enqueue (lambda (priority element)
                                                                                    (insert-after! (find-prev-cell priority) (make-item priority element))
                                                                                    true))
                                                                         (dequeue (lambda ()
                                                                                    (if (null? (content))
                                                                                       false
                                                                                       (let ((temp (car (content))))
                                                                                          (set-cdr! front (cdr (content)))
                                                                                          (get-element temp)))))
                                                                         (serve (lambda ()
                                                                                  (if (null? (content))
                                                                                     false
                                                                                     (get-element (car (content))))))
                                                                         (dispatch (lambda (m)
                                                                                     (if (eq? m 'empty?)
                                                                                        empty?
                                                                                        (if (eq? m 'enqueue)
                                                                                           enqueue
                                                                                           (if (eq? m 'dequeue)
                                                                                              dequeue
                                                                                              (if (eq? m 'serve)
                                                                                                 serve
                                                                                                 (error "unknown request
                 -- create-priority-queue" m))))))))
                                                                   dispatch))))
                                    (pq (create-priority-queue)))
                              ((pq 'enqueue) 66 'Patrick)
                              ((pq 'enqueue) -106 'Octo)
                              ((pq 'enqueue) 0 'Sandy)
                              ((pq 'enqueue) 89 'Spongebob)
                              ((pq 'dequeue))
                              (equal? ((pq 'dequeue)) 'Patrick)
                              (letrec ((copy (lambda (from-vector to-vector from-index to-index)
                                               (vector-set! to-vector to-index (vector-ref from-vector from-index))))
                                       (move (lambda (from-vector to-vector from-low from-high to-index)
                                               (letrec ((move-iter (lambda (n)
                                                                     (if (<= (+ from-low n) from-high)
                                                                        (begin
                                                                           (copy from-vector to-vector (+ from-low n) (+ to-index n))
                                                                           (move-iter (+ n 1)))
                                                                        #f))))
                                                  (move-iter 0))))
                                       (merge (lambda (vector1 vector2 vector low1 high1 low2 high2 to-index)
                                                (letrec ((merge-iter (lambda (index index1 index2)
                                                                       (if (> index1 high1)
                                                                          (move vector2 vector index2 high2 index)
                                                                          (if (> index2 high2)
                                                                             (move vector1 vector index1 high1 index)
                                                                             (if (< (vector-ref vector1 index1) (vector-ref vector2 index2))
                                                                                (begin
                                                                                   (copy vector1 vector index1 index)
                                                                                   (merge-iter (+ index 1) (+ index1 1) index2))
                                                                                (begin
                                                                                   (copy vector2 vector index2 index)
                                                                                   (merge-iter (+ index 1) index1 (+ index2 1)))))))))
                                                   (merge-iter to-index low1 low2))))
                                       (bottom-up-merge-sort (lambda (vector)
                                                               (letrec ((merge-subs (lambda (len)
                                                                                      (let ((aux-vector (make-vector (vector-length vector) 0)))
                                                                                         (letrec ((merge-subs-iter (lambda (index)
                                                                                                                     (if (< index (- (vector-length vector) (* 2 len)))
                                                                                                                        (begin
                                                                                                                           (merge vector vector aux-vector index (+ index len -1) (+ index len) (+ index len len -1) index)
                                                                                                                           (move aux-vector vector index (+ index len len -1) index)
                                                                                                                           (merge-subs-iter (+ index len len)))
                                                                                                                        (if (< index (- (vector-length vector) len))
                                                                                                                           (begin
                                                                                                                              (merge
                                                                                                                                 vector
                                                                                                                                 vector
                                                                                                                                 aux-vector
                                                                                                                                 index
                                                                                                                                 (+ index len -1)
                                                                                                                                 (+ index len)
                                                                                                                                 (- (vector-length vector) 1)
                                                                                                                                 index)
                                                                                                                              (move aux-vector vector index (- (vector-length vector) 1) index))
                                                                                                                           #f)))))
                                                                                            (merge-subs-iter 0)))))
                                                                        (merge-sort-iter (lambda (len)
                                                                                           (if (< len (vector-length vector))
                                                                                              (begin
                                                                                                 (merge-subs len)
                                                                                                 (merge-sort-iter (* 2 len)))
                                                                                              #f))))
                                                                  (merge-sort-iter 1)))))
                                 (let ((aVector (vector 8 3 6 6 0 5 4 2 9 6)))
                                    (bottom-up-merge-sort aVector)
                                    (equal? aVector (vector 0 2 3 4 5 6 6 6 8 9)))
                                 (letrec ((quick-sort2 (lambda (vector)
                                                         (letrec ((swap (lambda (v index1 index2)
                                                                          (let ((temp (vector-ref v index1)))
                                                                             (vector-set! v index1 (vector-ref v index2))
                                                                             (vector-set! v index2 temp))))
                                                                  (quick-sort-aux (lambda (low high)
                                                                                    (letrec ((quick-sort-aux-iter (lambda (mid-value from to)
                                                                                                                    (letrec ((quick-right (lambda (index1)
                                                                                                                                            (if (if (< index1 high) (< (vector-ref vector index1) mid-value) #f)
                                                                                                                                               (quick-right (+ index1 1))
                                                                                                                                               index1)))
                                                                                                                             (quick-left (lambda (index2)
                                                                                                                                           (if (if (> index2 low) (> (vector-ref vector index2) mid-value) #f)
                                                                                                                                              (quick-left (- index2 1))
                                                                                                                                              index2))))
                                                                                                                       (let ((index1 (quick-right (+ from 1)))
                                                                                                                             (index2 (quick-left to)))
                                                                                                                          (if (< index1 index2)
                                                                                                                             (begin
                                                                                                                                (swap vector index1 index2)
                                                                                                                                (quick-sort-aux-iter mid-value index1 index2))
                                                                                                                             index2))))))
                                                                                       (if (< low high)
                                                                                          (let ((middle (quotient (+ low high) 2))
                                                                                                (pivot-index (+ low 1)))
                                                                                             (swap vector middle pivot-index)
                                                                                             (if (> (vector-ref vector pivot-index) (vector-ref vector high))
                                                                                                (swap vector pivot-index high)
                                                                                                #f)
                                                                                             (if (> (vector-ref vector low) (vector-ref vector high))
                                                                                                (swap vector low high)
                                                                                                #f)
                                                                                             (if (< (vector-ref vector pivot-index) (vector-ref vector low))
                                                                                                (swap vector pivot-index low)
                                                                                                #f)
                                                                                             (let ((mid-index (quick-sort-aux-iter (vector-ref vector pivot-index) (+ low 1) high)))
                                                                                                (swap vector mid-index pivot-index)
                                                                                                (quick-sort-aux low (- mid-index 1))
                                                                                                (quick-sort-aux (+ mid-index 1) high)))
                                                                                          #f)))))
                                                            (quick-sort-aux 0 (- (vector-length vector) 1)))))
                                          (test3 (vector 8 3 6 6 1 5 4 2 9 6)))
                                    (quick-sort2 test3)
                                    (equal? test3 (vector 1 2 3 4 5 6 6 6 8 9))
                                    (letrec ((create-stack (lambda (eq-fnct)
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
                                                                   dispatch)))))
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
                                             #f)))))))))))))))