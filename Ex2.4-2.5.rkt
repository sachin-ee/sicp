#lang simply-scheme

;;; 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define x (cons 1 2))

(car x)
(cdr x)

;;; 2.5

(define (cons-new a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-new x)
  (define (car-iter x cnt)
    (if (= (remainder x 2) 0)
        (car-iter (/ x 2) (+ cnt 1))
        cnt))
  (car-iter x 0))

(define (cdr-new x)
  (define (cdr-iter x cnt)
    (if (= (remainder x 3) 0)
        (cdr-iter (/ x 3) (+ cnt 1))
        cnt))
  (cdr-iter x 0))

(define p1 (cons-new 3 5))
(car-new p1)
(cdr-new p1)