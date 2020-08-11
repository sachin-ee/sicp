#lang simply-scheme

;;; 1.37

(define (cont-frac-rec n d k)
  (if (= k 0)
      0
      (/ (n k) (+ (d k) (cont-frac-rec n d (- k 1))))))

;(trace cont-frac-rec)

(cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 11)
;(cont-frac 1.0 1.0 5)


(define (cont-frac-iter n d k)
  (define (iter n d k res cnt)
    (if (= k 0)
        res
        (iter n d (- k 1) (/ (n k) (+ (d k) res)) (+ cnt 1))))
  (iter n d k 0 1))

(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)

;;; 1.38

(define (cont-frac-iter-euler n d k)
  (define (iter n d k res cnt)
    (if (= k 0)
        res
        (iter n d (- k 1)
              (/ (n k) (+ (d (if (= (remainder cnt 3) 2)
                                 (* (+ 1 (/ cnt 3)) 2)
                                 1)) res))
              (+ cnt 1))))
  (iter n d k 0 1))

(define (euler) (+ 2.0 (cont-frac-iter-euler (lambda (i) 1.0) (lambda (i) i) 1000)))
(euler)

(define euler2 (+ 2.0 (cont-frac-iter-euler (lambda (i) 1.0) (lambda (i) i) 1000)))
euler2

;;; 1.39

(define (tan-cf x k)
  (cont-frac-rec (lambda (i) (if (= i 1000)
                                 x
                                 (- (* x x))))
                 (lambda (i) (+ (* (- 1000 i) 2) 1))
                 1000.0))

(tan-cf 90 1000)
                 