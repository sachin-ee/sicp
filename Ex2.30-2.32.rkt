#lang simply-scheme

;;; 2.30

(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) ((lambda (x) (* x x)) tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             ((lambda (x) (* x x)) sub-tree)
             (square-tree-map sub-tree)))
       tree))

(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;;; 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (proc sub-tree)
             (square-tree-map sub-tree)))
       tree))

(define (square x) (* x x))
(define (square-tree-new tree) (tree-map square tree))

(square-tree-new
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;;; 2.32

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))

(subsets (list 1 2 3))