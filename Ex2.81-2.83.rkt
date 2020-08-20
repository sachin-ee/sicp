#lang simply-scheme

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;; 2.81

;; a.
;; Infinite recursion

;; b.
;; Works just as is and could try raising both args to higher level and try apply-generic

;; c.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (and (= (length args) 2) (not (equal? type1 type2)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags)))))
                (error "No method for these types"
                       (list op type-tags))))))))

;;; 2.82

(define (get-coer-list args)
  (map (lambda (x)
         (map (lambda (y)
                (if (eq? y x)
                    (lambda (z) z)
                    (get-coercion y x)))
              args))
       args))

(define (true-list? li)
  (if (null? li)
      #t
      (if (car li)
          (true-list? (cdr li))
          #f)))

(define (apply-generic op . args)
  (define (iter it)
    (if (null? it)
        (error "No method for these types" (list op it))
        (let ((coer-list (car it)))
          (if (true-list? coer-list)
              (apply-generic op (map (lambda (x y) (x y)) coer-list args))
              (iter (cdr it))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (iter (get-coer-list type-tags))))))

;;; 2.83

(define (install-scheme-number-package)
  (define (raise x)
    ((get-coercion 'scheme-number 'rational) x))
  
  (put 'raise 'scheme-number
       (lambda (x) (raise x)))
  (put-coercion 'scheme-number 'rational
                (lambda (x)
                  (make-rat x 1)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (raise x)
    ((get-coercion 'rational 'real) x))
  
  ;; interface to rest of the system
  (put 'raise 'rational
       (lambda (x) (raise x)))
  (put-coercion 'rational 'real
                (lambda (x)
                  (/ (numer x) (denom x))))
  'done)

(define (install-real-package)
  ;; internal procedures
  (define (raise x)
    ((get-coercion 'real 'complex) x))
  
  ;; interface to rest of the system
  (put 'raise 'real
       (lambda (x) (raise x)))
  (put-coercion 'real 'complex
                (lambda (x)
                  (make-complex-from-real-img x 0)))
  'done)

(define (install-complex-package)
  ;; internal procedures

  
  ;; interface to rest of the system

  'done)


(define (raise x)
  (apply-generic 'raise x))
