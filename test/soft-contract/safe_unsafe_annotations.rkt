; this file is used to test the use of annotations to indicate false positives and true positives

; we have a true positive when we validate something as being safe,
; and it is safe, as well as something that we validate as unsafe and
; is indeed unsafe.
;
; If we validate something as unsafe while it is actually safe, then
; we have a false positive. If we validate something as safe while it
; is unsafe, than our analysis is unsound and we have discovered a bug.

(define x OPQ)

; the @unsafe means that the contract is not satisfied at runtime
(mon @unsafe int? x)

; the @safe means that the contract should be satisfied at runtime
(mon @safe any? x)

; @safe in this context means that the lambda we are monitoring is never violating the contract
(define f (mon @safe (~> int? int?) (lambda (x) x)))

; this gets turned into an application with annotation @unsafe
; this means that we cannot verify the input of the function, in that case a blame is generated on the location of the call
(@unsafe f OPQ)

; however this should be fine
(@safe f 3)
