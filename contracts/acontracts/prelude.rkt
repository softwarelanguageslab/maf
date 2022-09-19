#lang racket

;; Provides useful functions to all programs requiring the library
(require acontracts/actors)

(define (every-seconds seconds lmb)
  (letrec ((scheduler 
             (actor (thd)
                 (terminate ()
                   (kill-thread thd)
                   (terminate)))))

    (create scheduler (thread (lambda () (let loop () (lmb) (loop) (sleep seconds)))))))



