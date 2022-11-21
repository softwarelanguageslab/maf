; Sink called in implicit context. Flow from x to san.
(define x #t)
(define x-s (source x))
(define san (sanitize x-s))
(if x-s
  (begin
    (sink san)
    (display san))
  #f)