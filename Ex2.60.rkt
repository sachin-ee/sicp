#lang simply-scheme

;;; 2.60

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (remove x set)
  (if (equal? x (car set))
      (cdr set)
      (remove x (append (cdr set) (list(car set))))))      

;(remove 1 (list 2 3 1 4 5))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) (remove (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(intersection-set (list 2 3 2 1 3 2 2) (list 3 3 2 3 2 2 2 2 3 2 3))
(union-set (list 2 3 2 1 3 2 2) (list 3 3 2 3 2 2 2 2 3 2 3))