#lang simply-scheme

;;; 3.63
;; Inefficient because cons-stream must re-generate the stream. So all previous streams generated must be computed
;; again

;; Yes. Both are inefficient without using memo-proc

;;; 3.64
(define (stream-limit S tolerace)
  (if (< (abs (- (stream-car S) (stream-car (stream-cdr S)))) tolerance)
      (stream-car (stream-cdr S))
      (stream-limit (stream-cdr S) tolerance)))

;;; 3.65
(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-series (+ n 1)))))

(define ln-stream
  (partial-sums (ln-summands 1)))

(display-stream (euler-transform ln-stream))
(display-stream (accelerated-sequence euler-transform ln-stream))