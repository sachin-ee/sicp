#lang simply-scheme

;;; 2.1

(define (make-rat-better n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (or (and (>= n 0) (>= d 0))
            (and (< n 0) (< d 0)))
        (cons (/ n g) (/ d g))
        (cons (- (/ n g)) (/ d g)))))
        
;;; 2.2

(define (average x y) (/ (+ x y) 2))

(define (make-segment sp ep) (cons sp ep))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment seg)
  (make-point (average (x-point (start-segment seg)) (x-point (end-segment seg)))
                (average (y-point (start-segment seg)) (y-point (end-segment seg)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define p1 (make-point 2 3))
(define p2 (make-point 4 5))
(define s1 (make-segment p1 p2))
(define mid (midpoint-segment s1))
(print-point mid)

