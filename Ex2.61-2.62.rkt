#lang simply-scheme

;;; 2.61

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (insert x set)
  (cond ((null? set) (cons x set))
        ((> x (car set)) (cons (car set) (insert x (cdr set))))
        (else (cons x set))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (insert x set)))

(adjoin-set 3 (list 1 2 4 5))

;;; 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (if (< (car set1) (car set2))
             (cons (car set1) (union-set (cdr set1) set2))
             (cons (car set2) (union-set set1 (cdr set2)))))
        (else (union-set (cdr set1) set2))))

(union-set (list 6 7 8 9 10) (list 1 2 3 4 5))