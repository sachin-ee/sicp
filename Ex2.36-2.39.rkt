#lang simply-scheme

;;; 2.36

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;;; 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(dot-product '(1 2 3 4) '(4 5 6 6))
(matrix-*-vector '((1 2 3 4) (4 5 6 6) (6 7 8 9)) '(1 2 3 4))
(transpose '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(matrix-*-matrix '((1 2 3) (4 5 6) (6 7 8)) '((1 2 3) (4 5 6) (6 7 8)))

;;; 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

;; (1/(2/3))
(fold-right / 1 (list 1 2 3))

;; ((1/2)/3)
(fold-left / 1 (list 1 2 3))

;; (list 1(list 2(list 3 null)))
;; (1 (2 (3 ())))
(fold-right list null (list 1 2 3))

;; (((null list 1) list 2) list 3)
;; (((() 1) 2) 3)
(fold-left list null (list 1 2 3))

;; Associative or commutative property of addition or multiplication
;; yields same results for fold-right and fold-left

(fold-right + 0 (list 4 2 3))
(fold-left + 0 (list 4 2 3))
(fold-right * 1 (list 4 2 3))
(fold-left * 1 (list 4 2 3))

;;; 2.39

(define (reverse-fr sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse-fl sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

(reverse-fr (list 1 2 3 4 5))
(reverse-fl (list 1 2 3 4 5))