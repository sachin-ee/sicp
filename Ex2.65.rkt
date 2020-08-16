#lang simply-scheme

;;; 2.65

(define false #f)
(define true #t)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (union-set-helper set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (if (< (car set1) (car set2))
             (cons (car set1) (union-set-helper (cdr set1) set2))
             (cons (car set2) (union-set-helper set1 (cdr set2)))))
        (else (union-set-helper (cdr set1) set2))))

(define (intersection-set-helper set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-helper (cdr set1)
                                                 (cdr set2))))
              ((< x1 x2)
               (intersection-set-helper (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-helper set1 (cdr set2)))))))


(define (union-set tree1 tree2)
  (let ((set1 (tree->list-1 tree1))
        (set2 (tree->list-1 tree2)))
    (list->tree (union-set-helper set1 set2))))

(define (intersection-set tree1 tree2)
  (let ((set1 (tree->list-1 tree1))
        (set2 (tree->list-1 tree2)))
    (list->tree (intersection-set-helper set1 set2))))

(define x (make-tree 7 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())) (make-tree 9 '() (make-tree 11 '() '()))))
(define y (make-tree 2 (make-tree 0 '() '()) (make-tree 6 (make-tree 4 '() '()) (make-tree 8 '() (make-tree 10 '() '())))))
(define z (make-tree 5 (make-tree 3 (make-tree 1 '() '()) '()) (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))

(union-set x y)
(intersection-set x y)

(union-set y z)
(intersection-set y z)

(union-set x z)
(intersection-set x z)