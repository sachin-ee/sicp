#lang simply-scheme

;;; 2.77

;; apply-generic is called twice. Once for complex type and one more for rectangular type.

;;; 2.78

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (if (number? datum)
          'scheme-number
          (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (if (number? datum)
          datum
          (error "Bad tagged datum: CONTENTS" datum))))

;;; 2.79 and 2.80

(define (install-scheme-number-package)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  
  ;; interface to rest of the system
  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put '=zero? '(rational)
       (lambda (x) (and (= (numer x) 0)
                        (= (denom x) 0))))
  'done)

(define (install-complex-package)
  ;; internal procedures
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= imag-part z1) (imag-part z2)))
  
  ;; interface to rest of the system
  (put 'equ? '(complex complex)
       (lambda (x y) (equ? x y)))
  (put '=zero? '(rational)
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0))))
  'done)


(define (install-generic-arithmetic-package)
  (define (equ? x y)
    (apply-generic 'equ? x y))
  (define (=zero? x)
    (apply-generic '=zero? x)))