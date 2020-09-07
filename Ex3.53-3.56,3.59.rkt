#lang simply-scheme

;;; 3.53
;; (1 2 4 8 16 32 64 ...)

;;; 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

;;; 3.55
(define (partial-sums S)
  (cons-stream (stream-car S) (add-stream (partial-sums S) (stream-cdr S))))

;;; 3.56
(define s1 (scale-stream S 2))
(define s2 (scale-stream S 3))
(define s3 (scale-stream S 5))

(define S (cons-stream 1 (merge s1 (merge s2 s3))))

;;; 3.59
;; a.
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series S)
  (mul-streams S (div-streams ones integers)))

;; b.
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (stream-map - (integrate-series sine-series))))