(define familieboom '(jan (piet (frans (tom)
                                       (roel))
                                (mie))
                          (bram (inge (bert (ina)
                                            (ilse))
                                      (bart))
                                (iris))
                          (joost (else (ilse)))))



(define (familiehoofd fam) (car fam))
(define (kinderen fam) (cdr fam))
(define (laatste-nakomeling? fam)
  (null? (kinderen fam)))

(define (verdeel-democratisch boom budget)
  (define (verdeel boom)
    (if (laatste-nakomeling? boom)
        1
        (+ 1 (verdeel-in (kinderen boom)))))

  (define (verdeel-in lst)
    (if (null? lst)
        0
        (+ (verdeel (car lst))
           (verdeel-in (cdr lst)))))
  (/ budget (verdeel-in (kinderen boom))))

(define (budget boom budget-list)
  (define (budget-hulp boom budget-list)
    (+ (car budget-list)
       (budget-hulp-in (kinderen boom) (cdr budget-list))))

  (define (budget-hulp-in bomen budget-list)
   (if (or (null? bomen)(null? budget-list))
        0
        (+ (budget-hulp    (car bomen) budget-list)
           (budget-hulp-in (cdr bomen) budget-list))))
  (budget-hulp-in (kinderen boom) budget-list))

(and (= (verdeel-democratisch familieboom 1500) 100)
     (= (budget familieboom '(100 50 20)) 650))