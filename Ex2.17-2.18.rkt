#lang simply-scheme

;;; 2.17

(define (last-pair items)
  (if (eq? (cdr items) '())  ;; (null? (cdr items)) doesn't work. Not sure why!
      (cons (car items) null)
      (last-pair (cdr items))))

(last-pair (list 23 72 149 34))

;;; 2.18

;;method-1
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (reverse items)
  (let ((len (- (length items) 1)))
    (define (iter items length)
      (if (= length (- 1))
          null
          (cons (list-ref items length) (iter items (- length 1)))))
    (iter items len)))

(reverse (list 23 72 149 34))

;; method-2
(define (append item1 item2)
  (if (null? item1)
      item2
      (cons (car item1) (append (cdr item1) item2))))

(define (reverse-new items)
  (if (null? items)
      null
      (append (reverse-new (cdr items)) (list (car items)))))
      
(reverse-new (list 23 72 149 34))