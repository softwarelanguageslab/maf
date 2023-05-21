(define result '())
(define display2 (lambda (i) (set! result (cons i result))))
(define newline2 (lambda () (set! result (cons 'newline result))))

(define VUBOrganigram
  '(VUB (academisch (rectoraat)
                    (faculteiten
                     (rechten (bachelor (ba-rechten)
                                        (ba-criminologie))
                              (master (ma-rechten)
                                      (ma-criminologie)))
                     (economie)))
        (administratief (personeel) (financien))))

(define (display-n n d)
  (if (> n 0)(begin (display2 d)(display-n (- n 1) d))))

(define (print-lijn aantalblanco tekst)
  (display-n aantalblanco " ")
  (display2 tekst)
  (newline2))

(define (label organigram)  (car organigram))
(define (takken organigram) (cdr organigram))

(define (organigram-member-in een-label organigrammen)
  (if (null? organigrammen)
      #f
      (or (organigram-member een-label (car organigrammen))
          (organigram-member-in een-label (cdr organigrammen)))))

(define (organigram-member een-label organigram)
  (if (eq? een-label (label organigram))
      organigram
      (organigram-member-in een-label (takken organigram))))

(define (print organigram)
  (define (print diepte organigram)
    (print-lijn diepte (label organigram))
    (for-each (lambda (organigram)
                (print (+ diepte 1) organigram))
              (takken organigram)))
  (print 0 organigram))

(define (print-vanaf organigram label)
  (let ((res (organigram-member label organigram)))
    (if res
        (print res)
        #f)))

(print-vanaf VUBOrganigram 'rechten)

(equal? result
        '(newline
          financien
          " "
          " "
          newline
          personeel
          " "
          " "))
