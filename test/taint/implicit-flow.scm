; Flow from password-visible to result.
(define crit #t)
(define password-visible (source crit))
(define password "foo")
(define (display-password)
  (if password-visible
    (let ((result (display password)))
      (sink result))
    (display "not allowed")))
(display-password)