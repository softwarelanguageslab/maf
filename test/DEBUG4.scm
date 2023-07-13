(letrec ((app (lambda (f)
                (lambda ()
                  (f app))))
         (fun (lambda (x) 'end)))
    (if ((app (lambda (ign) #f)))
      ((app fun))
      (<change>
        ((app fun))
        '())))

; Reductie van "test/changes/scheme/generated/R5RS_WeiChenRompf2019_church_simple-4.scm". Incrementele analyse all opt niet precies.
;(define (a b) (lambda (c) (b c)))
;(define (d e) ((e (lambda (f) #f)) #t))
;(define (h i)
;  (define j (lambda (h) (h #t)))
;  (if (d i)
;      ((a j) i)
;      (<change>
;       ((a j) i)
;       '())))