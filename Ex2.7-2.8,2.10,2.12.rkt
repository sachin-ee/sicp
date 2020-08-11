#lang simply-scheme

(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;;; 2.7

(define (upper-bound interval) (max (car interval) (cdr interval)))
(define (lower-bound interval) (min (car interval) (cdr interval)))

;;; 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

;;; 2.10

(define (div-interval-zero x y)
  (if (and (< (lower-bound y) 0)
           (> (upper-bound y) 0))
      (error "Second interval spans 0")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define i1 (make-interval 1 2))
(define i2 (make-interval -1 1))
(div-interval i1 i2)
;(div-interval-zero i1 i2)

;;; 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center per-tolerance)
  (let ((tolerance (* center (/ per-tolerance 100.0))))
    (make-interval (- center tolerance)
                   (+ center tolerance))))

(define (percent interval)
  (* (/ (abs (- (center interval) (upper-bound interval))) (center interval)) 100.0))

(define i3 (make-center-percent 1 10))
(upper-bound i3)
(lower-bound i3)

(percent i3)