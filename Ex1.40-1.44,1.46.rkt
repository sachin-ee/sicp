#lang simply-scheme

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


;;; 1.40

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 2 3) 1)

;;; 1.41

(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

((double inc) 1)
(((double (double double)) inc) 5)

;;; 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

;;; 1.43

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;(trace repeated)

((repeated square 2) 5)

;;; 1.44

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx))) 3)))

(((repeated smooth 2) cos) 1.0)

;;; 1.46

(define (average x y)
  (/ (+ x y) 2))

(define (iterative-improve good-enough-func improve-guess-func)
  (lambda (guess)
    (if (good-enough-func guess)
        guess
        ((iterative-improve good-enough-func improve-guess-func) (improve-guess-func guess)))))

(define (sqrt-new x)
  ((iterative-improve (lambda (y) (< (abs (- (square y) x)) 0.001)) (lambda (y) (average y (/ x y)))) 1.0))

(sqrt-new 9)

(define (fixed-point-new f first-guess)
  ((iterative-improve (lambda (y) (< (abs (- y (f y))) tolerance))
                      (lambda (y) (f y))) 1.0))

(fixed-point-new cos 1.0)