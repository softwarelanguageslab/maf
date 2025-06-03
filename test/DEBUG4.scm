; Reductions of imprecise programs

; "test/changes/scheme/generated/R5RS_WeiChenRompf2019_the-little-schemer_ch3-5.scm" v
(letrec ((first (lambda ()
                  (<change>
                    ()
                    (cons () (first))))))
  (first))

;"test/changes/scheme/generated/R5RS_gabriel_puzzle-4.scm" v
(letrec ((for-each (lambda (f l)
                     (f (car l))))
          (*p* (make-vector 1)))
  (for-each
    (lambda (i)
      (<change>
        (vector-set! *p* i (make-vector 1))
        (vector-set! *p* i 1)))
    (cons 1 ())))

;"test/changes/scheme/generated/R5RS_scp1_all-but-interval-5.scm" v
(letrec ((equal? (lambda (a)
                   (let ((__or_res (eq? a #f)))
                     (if __or_res () #f))))
          (_0 (letrec ((all-but-interval (lambda ()
                                           (<change>
                                             ()
                                             #f))))
                (equal? (all-but-interval)))))
  ())

;"test/changes/scheme/generated/R5RS_scp1_count-pairs2-1.scm" v
;(letrec ((count-pairs (lambda (lst)
;                        (letrec ((count (lambda (current)
;                                          (count current))))
;                          (count lst))))
;          (retno (<change>
;                   ()
;                   #f)))
;  (count-pairs retno))

(letrec ((fun (lambda (arg)
                (letrec ((loop (lambda (curr)
                                 (loop curr))))
                  (loop arg))))
          (input (<change>
                   ()
                   #f)))
  (fun input))

; "test/changes/scheme/generated/R5RS_scp1_dedouble-2.scm"
(letrec ((ontdubbel-iter (lambda (restLijst)
                           (if restLijst
                             (<change>
                               (lambda unique_args_149
                                 ())
                               ())
                             (ontdubbel-iter ())))))
  (ontdubbel-iter #f))

; "test/changes/scheme/generated/R5RS_scp1_deep-map-combine-4.scm" v
(letrec ((__toplevel_cons cons)
          (_0 (letrec ((deep-combine (lambda (l)
                                       (if (null? l)
                                         (deep-combine (cdr l))
                                         (<change>
                                           (deep-combine (cdr l))
                                           #t)))))
                (deep-combine (__toplevel_cons 9 ())))))
  ())

;"test/changes/scheme/generated/R5RS_scp1_merge-1.scm" v
(letrec ((first-el (lambda ()
                     (if () 'S #f)))
          (merge (letrec ((merge-in (lambda (curr1)
                                      (if (null? curr1) #f (if (<change> #f ()) () ())))))
                   (let* ((result (first-el))
                           (curr1 result))
                     (merge-in curr1)))))
  ())

;"test/changes/scheme/generated/R5RS_scp1_merge-3.scm" v
;!semantiek afwijkend tijdens reductie
(letrec ((not (lambda ()
                #f))
          (__toplevel_cons cons))
  (letrec ((smaller? (lambda ()
                       (string<? (symbol->string 'S) (symbol->string 'S))))
            (merge (lambda (best1)
                     (letrec ((merge-in (lambda (curr1 prev)
                                          (begin
                                            (<change>
                                              curr1
                                              (not))
                                            (begin
                                              (<change>
                                                (set-cdr! prev curr1)
                                                merge-in)
                                              (<change>
                                                (merge-in cdr curr1)
                                                (set-cdr! prev curr1)))))))
                       (let* ((result (if (smaller?)
                                        (<change>
                                          best1
                                          #f)
                                        (<change>
                                          #f
                                          best1)))
                               (curr1 (cdr best1)))
                         (merge-in curr1 result)))))
            (best1 (__toplevel_cons 'Brussel ())))
    (merge best1)))

;"test/changes/scheme/generated/R5RS_scp1_merge-5.scm" v
(letrec ((not (lambda (x)
                (if x #f #t)))
          (_0 (letrec ((first-el (lambda ()
                                   (not ())))
                        (merge (if (<change> #f (not #f)) () (first-el))))
                ())))
  ())

; Verdere reductie met SCA refinement: v
(letrec ((not (lambda (x)
                (if x #f #t))))
  (if (<change> #f (not #f))
    ()
    (not ())))

;"test/changes/scheme/generated/R5RS_sigscheme_mem-1.scm"
(letrec ((foo (lambda ()
                (<change>
                  (lambda unique_args_5
                    ())
                  (foo)))))
  (foo))

;"test/changes/scheme/generated/R5RS_various_church-4.scm" v
(letrec ((plus (lambda (n1 n2)
                 (lambda (f)
                   (lambda (x)
                     (f ((n2 f) x))))))
          (mult (lambda (n1 n2)
                  (lambda (f)
                    (n2 (n1 f)))))
          (pred (lambda (n)
                  (<change>
                    (lambda (x)
                      id)
                    id)))
          (sub (lambda (n2)
                 (n2 pred)))
          (church0? (lambda (n)
                      ((n (lambda (x) #f)) #t)))
          (church=? (lambda (n1 n2)
                      (begin
                        (church0? n2)
                        (sub church1))))
          (church1 (lambda (f)
                     (lambda (x)
                       (f x))))
          (church2 (lambda (f)
                     (lambda (x)
                       (f x)))))
  (church=? (mult church2 (plus church1 #t)) (plus (mult church2 church1) (mult church2 #t))))

;"test/changes/scheme/generated/R5RS_various_four-in-a-row-5.scm"
(letrec ((fill-vector-iter! (lambda ()
                              (<change>
                                ()
                                (fill-vector-iter!))
                              ())))
  (fill-vector-iter!))