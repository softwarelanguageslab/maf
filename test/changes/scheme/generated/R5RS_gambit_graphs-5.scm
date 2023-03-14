; Changes:
; * removed: 0
; * added: 5
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 2
(letrec ((fold (lambda (lst folder state)
                 (<change>
                    ()
                    (display state))
                 (letrec ((__do_loop (lambda (lst state)
                                       (if (null? lst)
                                          state
                                          (__do_loop (cdr lst) (folder (car lst) state))))))
                    (__do_loop lst state))))
         (proc->vector (lambda (size f)
                         (if (zero? size)
                            (vector)
                            (let ((x (make-vector size (f 0))))
                               ((letrec ((loop (lambda (i)
                                                (if (< i size)
                                                   (begin
                                                      (vector-set! x i (f i))
                                                      (loop (+ i 1)))
                                                   #f))))
                                  loop)
                                  1)
                               x))))
         (vector-fold (lambda (vec folder state)
                        (let ((len (vector-length vec)))
                           (letrec ((__do_loop (lambda (i state)
                                                 (if (= i len)
                                                    state
                                                    (__do_loop (+ i 1) (folder (vector-ref vec i) state))))))
                              (__do_loop 0 state)))))
         (vector-map (lambda (vec proc)
                       (proc->vector (vector-length vec) (lambda (i) (proc (vector-ref vec i))))))
         (giota (lambda (limit)
                  ((letrec ((_-*- (lambda (limit res)
                                   (if (zero? limit)
                                      res
                                      (let ((limit (- limit 1)))
                                         (_-*- limit (cons limit res)))))))
                     _-*-)
                     limit
                     ())))
         (gnatural-fold (lambda (limit folder state)
                          (<change>
                             ()
                             i)
                          (letrec ((__do_loop (lambda (i state)
                                                (<change>
                                                   (if (= i limit)
                                                      state
                                                      (__do_loop (+ i 1) (folder i state)))
                                                   ((lambda (x) x) (if (= i limit) state (__do_loop (+ i 1) (folder i state))))))))
                             (__do_loop 0 state))))
         (gnatural-for-each (lambda (limit proc!)
                              (letrec ((__do_loop (lambda (i)
                                                    (<change>
                                                       ()
                                                       i)
                                                    (if (= i limit)
                                                       (<change>
                                                          #f
                                                          (begin
                                                             (proc! i)
                                                             (__do_loop (+ i 1))))
                                                       (<change>
                                                          (begin
                                                             (proc! i)
                                                             (__do_loop (+ i 1)))
                                                          #f)))))
                                 (__do_loop 0))))
         (natural-for-all? (lambda (limit ok?)
                             ((letrec ((_-*- (lambda (i)
                                              (let ((__or_res (= i limit)))
                                                 (if __or_res
                                                    __or_res
                                                    (if (ok? i) (_-*- (+ i 1)) #f))))))
                                _-*-)
                                0)))
         (natural-there-exists? (lambda (limit ok?)
                                  ((letrec ((_-*- (lambda (i)
                                                   (if (not (= i limit))
                                                      (let ((__or_res (ok? i)))
                                                         (if __or_res __or_res (_-*- (+ i 1))))
                                                      #f))))
                                     _-*-)
                                     0)))
         (there-exists? (lambda (lst ok?)
                          ((letrec ((_-*- (lambda (lst)
                                           (if (not (null? lst))
                                              (let ((__or_res (ok? (car lst))))
                                                 (if __or_res __or_res (_-*- (cdr lst))))
                                              #f))))
                             _-*-)
                             lst)))
         (fold-over-perm-tree (lambda (universe b-folder b-state t-folder t-state)
                                ((letrec ((_-*- (lambda (universe b-state t-state accross)
                                                 (if (null? universe)
                                                    (t-folder b-state t-state accross)
                                                    ((letrec ((_-**- (lambda (in out t-state)
                                                                      (let* ((first (car in))
                                                                             (rest (cdr in))
                                                                             (accross (if (null? rest)
                                                                                        accross
                                                                                        (lambda (new-t-state)
                                                                                           (_-**- rest (cons first out) new-t-state)))))
                                                                         (b-folder
                                                                            first
                                                                            b-state
                                                                            t-state
                                                                            (lambda (new-b-state new-t-state)
                                                                               (_-*- (fold out cons rest) new-b-state new-t-state accross))
                                                                            accross)))))
                                                       _-**-)
                                                       universe
                                                       ()
                                                       t-state)))))
                                   _-*-)
                                   universe
                                   b-state
                                   t-state
                                   (lambda (final-t-state)
                                      final-t-state))))
         (make-minimal? (lambda (max-size)
                          (let ((iotas (proc->vector (+ max-size 1) giota))
                                (perm (make-vector max-size 0)))
                             (lambda (size graph folder state)
                                (fold-over-perm-tree
                                   (vector-ref iotas size)
                                   (lambda (perm-x x state deeper accross)
                                      (let ((__case-atom-key (cmp-next-vertex graph perm x perm-x)))
                                         (if (eq? __case-atom-key 'less)
                                            #f
                                            (if (eq? __case-atom-key 'equal)
                                               (begin
                                                  (vector-set! perm x perm-x)
                                                  (deeper (+ x 1) state))
                                               (if (eq? __case-atom-key 'more)
                                                  (accross state)
                                                  (error "???"))))))
                                   0
                                   (lambda (leaf-depth state accross)
                                      (folder perm state accross))
                                   state)))))
         (cmp-next-vertex (lambda (graph perm x perm-x)
                            (let ((from-x (vector-ref graph x))
                                  (from-perm-x (vector-ref graph perm-x)))
                               ((letrec ((_-*- (lambda (y)
                                                (if (= x y)
                                                   'equal
                                                   (let ((x->y? (vector-ref from-x y))
                                                         (perm-y (vector-ref perm y)))
                                                      (if (eq? x->y? (vector-ref from-perm-x perm-y))
                                                         (let ((y->x? (vector-ref (vector-ref graph y) x)))
                                                            (if (eq? y->x? (vector-ref (vector-ref graph perm-y) perm-x))
                                                               (_-*- (+ y 1))
                                                               (if y->x? 'less 'more)))
                                                         (if x->y? 'less 'more)))))))
                                  _-*-)
                                  0))))
         (fold-over-rdg (lambda (size max-out folder state)
                          (let* ((root (- size 1))
                                 (edge? (proc->vector size (lambda (from) (make-vector size #f))))
                                 (edges (make-vector size ()))
                                 (out-degrees (make-vector size 0))
                                 (minimal-folder (make-minimal? root))
                                 (non-root-minimal? (let ((cont (lambda (perm state accross)
                                                                 (accross #t))))
                                                      (lambda (size)
                                                         (minimal-folder size edge? cont #t))))
                                 (root-minimal? (let ((cont (lambda (perm state accross)
                                                             (let ((__case-atom-key (cmp-next-vertex edge? perm root root)))
                                                                (if (eq? __case-atom-key 'less)
                                                                   #f
                                                                   (if (let ((__or_res (eq? __case-atom-key 'equal))) (if __or_res __or_res (eq? __case-atom-key 'more)))
                                                                      (accross #t)
                                                                      (error "???")))))))
                                                  (lambda ()
                                                     (minimal-folder root edge? cont #t)))))
                             ((letrec ((_-*- (lambda (vertex state)
                                              (if (not (non-root-minimal? vertex))
                                                 state
                                                 (if (= vertex root)
                                                    (let ((reach? (make-reach? root edges))
                                                          (from-root (vector-ref edge? root)))
                                                       ((letrec ((_-*- (lambda (v outs efr efrr state)
                                                                        (if (not (let ((__or_res (= v root))) (if __or_res __or_res (= outs max-out))))
                                                                           (begin
                                                                              (vector-set! from-root v #t)
                                                                              (let ((state (_-*- (+ v 1) (+ outs 1) (cons v efr) (cons (vector-ref reach? v) efrr) state)))
                                                                                 (vector-set! from-root v #f)
                                                                                 (_-*- (+ v 1) outs efr efrr state)))
                                                                           (if (if (natural-for-all? root (lambda (v) (there-exists? efrr (lambda (r) (vector-ref r v))))) (root-minimal?) #f)
                                                                              (begin
                                                                                 (vector-set! edges root efr)
                                                                                 (folder (proc->vector size (lambda (i) (vector-ref edges i))) state))
                                                                              state)))))
                                                          _-*-)
                                                          0
                                                          0
                                                          ()
                                                          ()
                                                          state))
                                                    (let ((from-vertex (vector-ref edge? vertex)))
                                                       ((letrec ((_-**- (lambda (sv outs state)
                                                                         (if (= sv vertex)
                                                                            (begin
                                                                               (vector-set! out-degrees vertex outs)
                                                                               (_-*- (+ vertex 1) state))
                                                                            (let* ((state (_-**- (+ sv 1) outs state))
                                                                                   (from-sv (vector-ref edge? sv))
                                                                                   (sv-out (vector-ref out-degrees sv))
                                                                                   (state (if (= sv-out max-out)
                                                                                            state
                                                                                            (begin
                                                                                               (vector-set! edges sv (cons vertex (vector-ref edges sv)))
                                                                                               (vector-set! from-sv vertex #t)
                                                                                               (vector-set! out-degrees sv (+ sv-out 1))
                                                                                               (let* ((state (_-**- (+ sv 1) outs state))
                                                                                                      (state (if (= outs max-out)
                                                                                                               state
                                                                                                               (begin
                                                                                                                  (vector-set! from-vertex sv #t)
                                                                                                                  (vector-set! edges vertex (cons sv (vector-ref edges vertex)))
                                                                                                                  (let ((state (_-**- (+ sv 1) (+ outs 1) state)))
                                                                                                                     (vector-set! edges vertex (cdr (vector-ref edges vertex)))
                                                                                                                     (vector-set! from-vertex sv #f)
                                                                                                                     state)))))
                                                                                                  (vector-set! out-degrees sv sv-out)
                                                                                                  (vector-set! from-sv vertex #f)
                                                                                                  (vector-set! edges sv (cdr (vector-ref edges sv)))
                                                                                                  state)))))
                                                                               (if (= outs max-out)
                                                                                  state
                                                                                  (begin
                                                                                     (vector-set! edges vertex (cons sv (vector-ref edges vertex)))
                                                                                     (vector-set! from-vertex sv #t)
                                                                                     (let ((state (_-**- (+ sv 1) (+ outs 1) state)))
                                                                                        (vector-set! from-vertex sv #f)
                                                                                        (vector-set! edges vertex (cdr (vector-ref edges vertex)))
                                                                                        state))))))))
                                                          _-**-)
                                                          0
                                                          0
                                                          state)))))))
                                _-*-)
                                0
                                state))))
         (make-reach? (lambda (size vertex->out)
                        (let ((res (proc->vector
                                     size
                                     (lambda (v)
                                        (let ((from-v (make-vector size #f)))
                                           (vector-set! from-v v #t)
                                           (<change>
                                              (for-each (lambda (x) (vector-set! from-v x #t)) (vector-ref vertex->out v))
                                              ((lambda (x) x) (for-each (lambda (x) (vector-set! from-v x #t)) (vector-ref vertex->out v))))
                                           from-v)))))
                           (gnatural-for-each
                              size
                              (lambda (m)
                                 (let ((from-m (vector-ref res m)))
                                    (gnatural-for-each
                                       size
                                       (lambda (f)
                                          (let ((from-f (vector-ref res f)))
                                             (<change>
                                                ()
                                                (display (if (not (vector-ref from-m t)) (vector-set! from-f t #t) #f)))
                                             (if (vector-ref from-f m)
                                                (gnatural-for-each size (lambda (t) (if (vector-ref from-m t) (vector-set! from-f t #t) #f)))
                                                #f)))))))
                           res)))
         (run (lambda (n)
                (fold-over-rdg n 2 cons ()))))
   (<change>
      ()
      596)
   (= (length (run 5)) 596))