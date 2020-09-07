#lang simply-scheme

;;; 3.67
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) t))))

;; or

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s))
     (pairs (stream-cdr s) (stream-cdr t))))))

;;; 3.68
;; Infinite recursion