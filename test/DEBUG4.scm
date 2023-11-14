(letrec ((app (lambda (f)
                (lambda ()
                  (f app))))
         (fun (lambda (x) 'end)))
    (if ((app (lambda (ign) #f)))
      ((app fun))
      (<change>
        ((app fun))
        '())))

; (letrec ((phi (lambda () (<change> #f "S"))) (_0 (let ((__or_res (phi))) (if __or_res () (phi))))) ())