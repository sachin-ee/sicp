#lang simply-scheme

;;; 3.73
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (define (RC-int i-stream v0)
    (add-stream (scale-stream i-stream R)
                (integral (scale-stream i-stream (/1 C)) v0 dt)))
  RC-int)

;;; 3.74
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (stream-cdr sense-data)))