#lang simply-scheme

;;; 1.7

(define (square x) (* x x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess old-guess x)
  (if (new-good-enough? guess old-guess x)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (new-good-enough? guess old-guess x)
  (< (abs (- old-guess guess)) (* guess 0.001)))

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))


;;; 1.8

(define (cubert-iter guess old-guess x)
  (if (cb-good-enough? guess old-guess x)
      guess
      (cubert-iter (cb-improve guess x) guess x)))

(define (cb-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cb-good-enough? guess old-guess x)
  (< (abs (- old-guess guess)) (* guess 0.001)))

(define (cbrt x)
  (cubert-iter 1.0 0.0 x))