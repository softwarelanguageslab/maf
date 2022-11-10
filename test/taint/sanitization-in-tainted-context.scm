; Sanitizer called in implicit context. Flow from x to san.
(define x #t)
(define x-s (source x))
(if x-s
  (let ((san (sanitize x-s)))
    (sink san)
    (display san))
  #f)