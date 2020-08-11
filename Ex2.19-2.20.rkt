#lang simply-scheme

;; 2.19

(define us-coins (list 50 25 10 5 1))
(define us-coins-2 (list 1 5 10 25 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (no-more? item)
  (null? item))

(define (except-first-denomination items)
  (cdr items))

(define (first-denomination items)
  (car items))

(cc 100 us-coins)
(cc 100 us-coins-2)
(cc 25 uk-coins)

;; 2.20

(define (same-parity first . items)
  (let ((parity (remainder first 2)))
    (define (iter item)
      (if (null? item)
          '()
          (if (eq? parity (remainder (car item) 2))
              (cons (car item) (iter (cdr item)))
              (iter (cdr item)))))
    (iter (cons first items))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)