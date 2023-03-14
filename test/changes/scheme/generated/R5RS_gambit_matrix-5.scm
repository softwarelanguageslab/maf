; Changes:
; * removed: 0
; * added: 3
; * swaps: 1
; * negated predicates: 2
; * swapped branches: 0
; * calls to id fun: 10
(letrec ((map2 (lambda (f l1 l2)
                 (if (let ((__or_res (null? l1))) (if __or_res __or_res (null? l2)))
                    ()
                    (if (if (pair? l1) (pair? l2) #f)
                       (cons (f (car l1) (car l2)) (map2 f (cdr l1) (cdr l2)))
                       (error "Cannot map2 over a non-list")))))
         (chez-box (lambda (x)
                     (cons x ())))
         (chez-unbox (lambda (x)
                       (car x)))
         (chez-set-box! (lambda (x y)
                          (set-car! x y)))
         (maximal? (lambda (mat)
                     ((letrec ((pick-first-row (lambda (first-row-perm)
                                                (if first-row-perm
                                                   (if (zunda first-row-perm mat)
                                                      (pick-first-row (first-row-perm 'brother))
                                                      #f)
                                                   #t))))
                        pick-first-row)
                        (gen-perms mat))))
         (zunda (lambda (first-row-perm mat)
                  (let* ((first-row (first-row-perm 'now))
                         (number-of-cols (length first-row))
                         (make-row->func (lambda (if-equal if-different)
                                           (<change>
                                              (lambda (row)
                                                 (let ((vec (make-vector number-of-cols 0)))
                                                    (letrec ((__do_loop (lambda (i first row)
                                                                          (if (= i number-of-cols)
                                                                             #f
                                                                             (begin
                                                                                (vector-set! vec i (if (= (car first) (car row)) if-equal if-different))
                                                                                (__do_loop (+ i 1) (cdr first) (cdr row)))))))
                                                       (__do_loop 0 first-row row))
                                                    (lambda (i)
                                                       (vector-ref vec i))))
                                              ((lambda (x) x)
                                                 (lambda (row)
                                                    (let ((vec (make-vector number-of-cols 0)))
                                                       (<change>
                                                          (letrec ((__do_loop (lambda (i first row)
                                                                                (if (= i number-of-cols)
                                                                                   #f
                                                                                   (begin
                                                                                      (vector-set! vec i (if (= (car first) (car row)) if-equal if-different))
                                                                                      (__do_loop (+ i 1) (cdr first) (cdr row)))))))
                                                             (__do_loop 0 first-row row))
                                                          (lambda (i)
                                                             (vector-ref vec i)))
                                                       (<change>
                                                          (lambda (i)
                                                             (vector-ref vec i))
                                                          (letrec ((__do_loop (lambda (i first row)
                                                                                (if (= i number-of-cols)
                                                                                   #f
                                                                                   (begin
                                                                                      (vector-set! vec i (if (= (car first) (car row)) if-equal if-different))
                                                                                      (__do_loop (+ i 1) (cdr first) (cdr row)))))))
                                                             (__do_loop 0 first-row row)))))))))
                         (mat (cdr mat)))
                     (zebra (first-row-perm 'child) (make-row->func 1 -1) (make-row->func -1 1) mat number-of-cols))))
         (zebra (lambda (row-perm row->func+ row->func- mat number-of-cols)
                  (<change>
                     ()
                     'child)
                  ((letrec ((_-*- (lambda (row-perm mat partitions)
                                   (let ((__or_res (not row-perm)))
                                      (if __or_res
                                         __or_res
                                         (if (zulu (car mat) (row->func+ (row-perm 'now)) partitions (lambda (new-partitions) (_-*- (row-perm 'child) (cdr mat) new-partitions)))
                                            (if (zulu (car mat) (row->func- (row-perm 'now)) partitions (lambda (new-partitions) (_-*- (row-perm 'child) (cdr mat) new-partitions)))
                                               (let ((new-row-perm (row-perm 'brother)))
                                                  (let ((__or_res (not new-row-perm)))
                                                     (if __or_res
                                                        __or_res
                                                        (_-*- new-row-perm mat partitions))))
                                               #f)
                                            #f))))))
                     _-*-)
                     row-perm
                     mat
                     (list (miota number-of-cols)))))
         (zulu (let ((cons-if-not-null (lambda (lhs rhs)
                                        (if (null? lhs) rhs (cons lhs rhs)))))
                 (lambda (old-row new-row-func partitions equal-cont)
                    ((letrec ((_-*- (lambda (p-in old-row rev-p-out)
                                     ((letrec ((_-split- (lambda (partition old-row plus minus)
                                                          (if (null? partition)
                                                             ((letrec ((_-minus- (lambda (old-row m)
                                                                                  (if (null? m)
                                                                                     (let ((rev-p-out (cons-if-not-null minus (cons-if-not-null plus rev-p-out)))
                                                                                           (p-in (cdr p-in)))
                                                                                        (if (null? p-in)
                                                                                           (equal-cont (reverse rev-p-out))
                                                                                           (_-*- p-in old-row rev-p-out)))
                                                                                     (let ((__or_res (= 1 (car old-row))))
                                                                                        (if __or_res
                                                                                           __or_res
                                                                                           (_-minus- (cdr old-row) (cdr m))))))))
                                                                _-minus-)
                                                                old-row
                                                                minus)
                                                             (let ((next (car partition)))
                                                                (let ((__case-atom-key (new-row-func next)))
                                                                   (if (eq? __case-atom-key 1)
                                                                      (if (= 1 (car old-row))
                                                                         (_-split- (cdr partition) (cdr old-row) (cons next plus) minus)
                                                                         #f)
                                                                      (if (eq? __case-atom-key -1)
                                                                         (_-split- (cdr partition) old-row plus (cons next minus))
                                                                         #f))))))))
                                        _-split-)
                                        (car p-in)
                                        old-row
                                        ()
                                        ()))))
                       _-*-)
                       partitions
                       old-row
                       ()))))
         (all? (lambda (ok? lst)
                 ((letrec ((_-*- (lambda (lst)
                                  (let ((__or_res (null? lst)))
                                     (if __or_res
                                        __or_res
                                        (if (ok? (car lst)) (_-*- (cdr lst)) #f))))))
                    _-*-)
                    lst)))
         (gen-perms (lambda (objects)
                      (<change>
                         ((letrec ((_-*- (lambda (zulu-future past)
                                          (if (null? zulu-future)
                                             #f
                                             (lambda (msg)
                                                (if (eq? msg 'now)
                                                   (car zulu-future)
                                                   (if (eq? msg 'brother)
                                                      (_-*- (cdr zulu-future) (cons (car zulu-future) past))
                                                      (if (eq? msg 'child)
                                                         (gen-perms (fold past cons (cdr zulu-future)))
                                                         (if (eq? msg 'puke)
                                                            (cons (car zulu-future) (fold past cons (cdr zulu-future)))
                                                            (error gen-perms "Bad msg: ~a" msg))))))))))
                            _-*-)
                            objects
                            ())
                         ((lambda (x) x)
                            ((letrec ((_-*- (lambda (zulu-future past)
                                             (if (null? zulu-future)
                                                #f
                                                (lambda (msg)
                                                   (if (eq? msg 'now)
                                                      (car zulu-future)
                                                      (if (eq? msg 'brother)
                                                         (_-*- (cdr zulu-future) (cons (car zulu-future) past))
                                                         (if (eq? msg 'child)
                                                            (gen-perms (fold past cons (cdr zulu-future)))
                                                            (if (eq? msg 'puke)
                                                               (cons (car zulu-future) (fold past cons (cdr zulu-future)))
                                                               (error gen-perms "Bad msg: ~a" msg))))))))))
                               _-*-)
                               objects
                               ())))))
         (fold (lambda (lst folder state)
                 ((letrec ((_-*- (lambda (lst state)
                                  (if (null? lst)
                                     state
                                     (_-*- (cdr lst) (folder (car lst) state))))))
                    _-*-)
                    lst
                    state)))
         (miota (lambda (len)
                  (<change>
                     ((letrec ((_-*- (lambda (i) (if (= i len) () (cons i (_-*- (+ i 1))))))) _-*-) 0)
                     ((lambda (x) x) ((letrec ((_-*- (lambda (i) (if (= i len) () (cons i (_-*- (+ i 1))))))) _-*-) 0)))))
         (proc->vector (lambda (size proc)
                         (let ((res (make-vector size 0)))
                            (letrec ((__do_loop (lambda (i)
                                                  (if (= i size)
                                                     #f
                                                     (begin
                                                        (vector-set! res i (proc i))
                                                        (__do_loop (+ i 1)))))))
                               (__do_loop 0))
                            res)))
         (make-modular (lambda (modulus)
                         (let* ((reduce (lambda (x)
                                          (modulo x modulus)))
                                (coef-zero? (lambda (x)
                                              (<change>
                                                 (zero? (reduce x))
                                                 ((lambda (x) x) (zero? (reduce x))))))
                                (coef-+ (lambda (x y)
                                          (reduce (+ x y))))
                                (coef-negate (lambda (x)
                                               (reduce (- x))))
                                (coef-* (lambda (x y)
                                          (<change>
                                             (reduce (* x y))
                                             ((lambda (x) x) (reduce (* x y))))))
                                (coef-recip (let ((inverses (proc->vector
                                                             (- modulus 1)
                                                             (lambda (i)
                                                                (extended-gcd (+ i 1) modulus (lambda (gcd inverse ignore) inverse))))))
                                              (lambda (x)
                                                 (<change>
                                                    (let ((x (reduce x)))
                                                       (vector-ref inverses (- x 1)))
                                                    ((lambda (x) x) (let ((x (reduce x))) (vector-ref inverses (- x 1)))))))))
                            (lambda (maker)
                               (maker 0 1 coef-zero? coef-+ coef-negate coef-* coef-recip)))))
         (extended-gcd (let ((n->sgn/abs (lambda (x cont)
                                          (if (>= x 0) (cont 1 x) (cons -1 (- x))))))
                         (lambda (a b cont)
                            (n->sgn/abs
                               a
                               (lambda (p-a p)
                                  (<change>
                                     (n->sgn/abs
                                        b
                                        (lambda (q-b q)
                                           ((letrec ((_-*- (lambda (p p-a p-b q q-a q-b)
                                                            (if (zero? q)
                                                               (cont p p-a p-b)
                                                               (let ((mult (quotient p q)))
                                                                  (_-*- q q-a q-b (- p (* mult q)) (- p-a (* mult q-a)) (- p-b (* mult q-b))))))))
                                              _-*-)
                                              p
                                              p-a
                                              0
                                              q
                                              0
                                              q-b)))
                                     ((lambda (x) x)
                                        (n->sgn/abs
                                           b
                                           (lambda (q-b q)
                                              ((letrec ((_-*- (lambda (p p-a p-b q q-a q-b)
                                                               (if (zero? q)
                                                                  (cont p p-a p-b)
                                                                  (let ((mult (quotient p q)))
                                                                     (_-*- q q-a q-b (- p (* mult q)) (- p-a (* mult q-a)) (- p-b (* mult q-b))))))))
                                                 _-*-)
                                                 p
                                                 p-a
                                                 0
                                                 q
                                                 0
                                                 q-b))))))))))
         (make-row-reduce (lambda (coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)
                            (lambda (mat)
                               ((letrec ((_-*- (lambda (mat)
                                                (if (let ((__or_res (null? mat))) (if __or_res __or_res (null? (car mat))))
                                                   ()
                                                   ((letrec ((_-**- (lambda (in out)
                                                                     (if (null? in)
                                                                        (map (lambda (x) (cons coef-zero x)) (_-*- out))
                                                                        (let* ((prow (car in))
                                                                               (pivot (car prow))
                                                                               (prest (cdr prow))
                                                                               (in (cdr in)))
                                                                           (if (coef-zero? pivot)
                                                                              (_-**- in (cons prest out))
                                                                              (let ((zap-row (map (let ((mult (coef-recip pivot))) (lambda (x) (coef-* mult x))) prest)))
                                                                                 (cons
                                                                                    (cons coef-one zap-row)
                                                                                    (map
                                                                                       (lambda (x)
                                                                                          (cons coef-zero x))
                                                                                       (_-*-
                                                                                          (fold
                                                                                             in
                                                                                             (lambda (row mat)
                                                                                                (cons
                                                                                                   (let ((first-col (car row))
                                                                                                         (rest-row (cdr row)))
                                                                                                      (if (coef-zero? first-col)
                                                                                                         rest-row
                                                                                                         (map2
                                                                                                            (let ((mult (coef-negate first-col)))
                                                                                                               (lambda (f z)
                                                                                                                  (coef-+ f (coef-* mult z))))
                                                                                                            rest-row
                                                                                                            zap-row)))
                                                                                                   mat))
                                                                                             out)))))))))))
                                                      _-**-)
                                                      mat
                                                      ())))))
                                  _-*-)
                                  mat))))
         (make-in-row-space? (lambda (coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)
                               (let ((row-reduce (make-row-reduce coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)))
                                  (lambda (mat)
                                     (let ((mat (row-reduce mat)))
                                        (lambda (row)
                                           ((letrec ((_-*- (lambda (row mat)
                                                            (if (null? row)
                                                               #t
                                                               (let ((r-first (car row))
                                                                     (r-rest (cdr row)))
                                                                  (if (coef-zero? r-first)
                                                                     (_-*-
                                                                        r-rest
                                                                        (map
                                                                           cdr
                                                                           (if (let ((__or_res (null? mat))) (if __or_res __or_res (coef-zero? (caar mat))))
                                                                              mat
                                                                              (cdr mat))))
                                                                     (if (null? mat)
                                                                        #f
                                                                        (let* ((zap-row (car mat))
                                                                               (z-first (car zap-row))
                                                                               (z-rest (cdr zap-row))
                                                                               (mat (cdr mat)))
                                                                           (if (coef-zero? z-first)
                                                                              #f
                                                                              (_-*-
                                                                                 (map2 (let ((mult (coef-negate r-first))) (lambda (r z) (coef-+ r (coef-* mult z)))) r-rest z-rest)
                                                                                 (map cdr mat)))))))))))
                                              _-*-)
                                              row
                                              mat)))))))
         (make-modular-row-reduce (lambda (modulus)
                                    ((make-modular modulus) make-row-reduce)))
         (make-modular-in-row-space? (lambda (modulus)
                                       (<change>
                                          ((make-modular modulus) make-in-row-space?)
                                          ((lambda (x) x) ((make-modular modulus) make-in-row-space?)))))
         (find-prime (lambda (bound)
                       (let* ((primes (list 2))
                              (last (chez-box primes))
                              (is-next-prime? (lambda (trial)
                                                ((letrec ((_-*- (lambda (primes)
                                                                 (let ((__or_res (null? primes)))
                                                                    (if __or_res
                                                                       __or_res
                                                                       (let ((p (car primes)))
                                                                          (let ((__or_res (< trial (* p p))))
                                                                             (if __or_res
                                                                                __or_res
                                                                                (if (not (zero? (modulo trial p)))
                                                                                   (_-*- (cdr primes))
                                                                                   #f)))))))))
                                                   _-*-)
                                                   primes))))
                          (if (> 2 bound)
                             2
                             ((letrec ((_-*- (lambda (trial)
                                              (if (is-next-prime? trial)
                                                 (let ((entry (list trial)))
                                                    (set-cdr! (chez-unbox last) entry)
                                                    (chez-set-box! last entry)
                                                    (if (> trial bound) trial (_-*- (+ trial 2))))
                                                 (_-*- (+ trial 2))))))
                                _-*-)
                                3)))))
         (det-upper-bound (lambda (size)
                            (let ((main-part (expt size (quotient size 2))))
                               (if (even? size)
                                  main-part
                                  (*
                                     main-part
                                     (letrec ((__do_loop (lambda (i)
                                                           (<change>
                                                              ()
                                                              (display i))
                                                           (if (>= (* i i) size) i (__do_loop (+ i 1))))))
                                        (__do_loop 0)))))))
         (go (lambda (number-of-cols inv-size folder state)
               (let* ((in-row-space? (make-modular-in-row-space? (find-prime (det-upper-bound inv-size))))
                      (make-tester (lambda (mat)
                                     (let ((tests (let ((old-mat (cdr mat))
                                                       (new-row (car mat)))
                                                    (fold-over-subs-of-size
                                                       old-mat
                                                       (- inv-size 2)
                                                       (lambda (sub tests)
                                                          (cons (in-row-space? (cons new-row sub)) tests))
                                                       ()))))
                                        (lambda (row)
                                           ((letrec ((_-*- (lambda (tests)
                                                            (if (not (null? tests))
                                                               (let ((__or_res ((car tests) row)))
                                                                  (if __or_res __or_res (_-*- (cdr tests))))
                                                               #f))))
                                              _-*-)
                                              tests)))))
                      (all-rows (fold
                                  (fold-over-rows (- number-of-cols 1) cons ())
                                  (lambda (row rows)
                                     (cons (cons 1 row) rows))
                                  ())))
                  ((letrec ((_-*- (lambda (number-of-rows rev-mat possible-future state)
                                   (let ((zulu-future (remove-in-order
                                                        (if (< number-of-rows inv-size)
                                                           (in-row-space? rev-mat)
                                                           (make-tester rev-mat))
                                                        possible-future)))
                                      (if (null? zulu-future)
                                         (folder (reverse rev-mat) state)
                                         ((letrec ((_-**- (lambda (zulu-future state)
                                                           (if (null? zulu-future)
                                                              state
                                                              (let ((rest-of-future (cdr zulu-future)))
                                                                 (_-**-
                                                                    rest-of-future
                                                                    (let* ((first (car zulu-future))
                                                                           (new-rev-mat (cons first rev-mat)))
                                                                       (if (maximal? (reverse new-rev-mat))
                                                                          (_-*- (+ number-of-rows 1) new-rev-mat rest-of-future state)
                                                                          state))))))))
                                            _-**-)
                                            zulu-future
                                            state))))))
                     _-*-)
                     1
                     (list (car all-rows))
                     (cdr all-rows)
                     state))))
         (go-folder (lambda (mat bsize.blen.blist)
                      (<change>
                         ()
                         (display cons))
                      (let ((bsize (car bsize.blen.blist))
                            (size (length mat)))
                         (if (<change> (< size bsize) (not (< size bsize)))
                            bsize.blen.blist
                            (let ((blen (cadr bsize.blen.blist))
                                  (blist (cddr bsize.blen.blist)))
                               (if (= size bsize)
                                  (let ((blen (+ blen 1)))
                                     (<change>
                                        (cons
                                           bsize
                                           (cons blen (if (< blen 3000) (cons mat blist) (if (= blen 3000) (cons "..." blist) blist))))
                                        ((lambda (x) x)
                                           (cons
                                              bsize
                                              (cons blen (if (< blen 3000) (cons mat blist) (if (= blen 3000) (cons "..." blist) blist)))))))
                                  (list size 1 mat)))))))
         (really-go (lambda (number-of-cols inv-size)
                      (cddr (go number-of-cols inv-size go-folder (list -1 -1)))))
         (remove-in-order (lambda (remove? lst)
                            (reverse
                               (fold lst (lambda (e lst) (if (<change> (remove? e) (not (remove? e))) lst (cons e lst))) ()))))
         (fold-over-rows (lambda (number-of-cols folder state)
                           (if (zero? number-of-cols)
                              (folder () state)
                              (fold-over-rows
                                 (- number-of-cols 1)
                                 (lambda (tail state)
                                    (folder (cons -1 tail) state))
                                 (fold-over-rows (- number-of-cols 1) (lambda (tail state) (folder (cons 1 tail) state)) state)))))
         (fold-over-subs-of-size (lambda (universe size folder state)
                                   (let ((usize (length universe)))
                                      (if (< usize size)
                                         state
                                         ((letrec ((_-*- (lambda (size universe folder csize state)
                                                          (if (zero? csize)
                                                             (folder universe state)
                                                             (if (zero? size)
                                                                (folder () state)
                                                                (let ((first-u (car universe))
                                                                      (rest-u (cdr universe)))
                                                                   (_-*-
                                                                      size
                                                                      rest-u
                                                                      folder
                                                                      (- csize 1)
                                                                      (_-*- (- size 1) rest-u (lambda (tail state) (folder (cons first-u tail) state)) csize state))))))))
                                            _-*-)
                                            size
                                            universe
                                            folder
                                            (- usize size)
                                            state))))))
   (<change>
      (equal?
         (really-go 5 5)
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons
                  1
                  (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
               (__toplevel_cons
                  (__toplevel_cons
                     1
                     (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                              ()))))))
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons
                     1
                     (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                 ()))))))
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                    ()))))))
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          1
                                          (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                                       ()))))))
                     (__toplevel_cons
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          1
                                          (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             1
                                             (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                                          ()))))))
                        ()))))))
      ((lambda (x) x)
         (equal?
            (really-go 5 5)
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons
                     1
                     (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                 ()))))))
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                    ()))))))
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          1
                                          (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                       ()))))))
                     (__toplevel_cons
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          1
                                          (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             1
                                             (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                                          ()))))))
                        (__toplevel_cons
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          1
                                          (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             1
                                             (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                1
                                                (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                                             ()))))))
                           ())))))))))