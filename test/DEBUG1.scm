(let* ((last (<change>
               ()
               (cons 'box ()))))
  (if =
    (set-cdr! ((lambda (x) x) last) ())))