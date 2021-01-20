; Modified from https://github.com/philnguyen/soft-contract/blob/a6a2963bb3d5826ca4405a257a6178873d046b76/soft-contract/test/programs/safe/mochi/fold-fun-list.rkt

(define (mk-list n)
  (assert (integer? n))
  (if (<= n 0)
      '()
      (mk-list (- n 1))))

(define (main n)
  (assert (integer? n))
  (mk-list n)
    #t)

(main (<change> 10.5 10))
