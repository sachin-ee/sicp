#lang simply-scheme

;;; 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-operator-package)
  (define (sum operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))

  (define (product operands var)
    (make-sum (make-product
               (car operands)
               (deriv (cadr operands) var))
              (make-product
               (deriv (car operands) var)
               (cadr operands))))

  (define (exponentiation operands var)
    (make-product (make-product (cadr operands)
                                (make-exponentiation (car operands)
                                                     (- (cadr operands) 1)))
                  (deriv (car operands) var)))

  (put 'deriv '+ (lambda (x y) (sum x y)))
  (put 'deriv '* (lambda (x y) (product x y)))
  (put 'deriv '** (lambda (x y) (exponentiation x y)))
  'done)

(define (install-operator-deriv-package)
  ;; same as install-deriv-operator-package and change the order in get
  (put '+ 'deriv (lambda (x y) (sum x y)))
  (put '* 'deriv (lambda (x y) (product x y)))
  (put '** 'deriv (lambda (x y) (exponentiation x y)))
  'done)