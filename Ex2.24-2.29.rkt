#lang simply-scheme

;;; 2.24
(list 1 (list 2 (list 3 4)))

;;; 2.25

(car (cdaddr '(1 3 (5 7) 9)))

(caar '((7)))

(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;;; 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

(cons 0 x)
(cons y 7)

;;; 2.27

(define (deep-reverse items)
  (cond ((null? items) null)
        (else (append (deep-reverse (cdr items)) (if (pair? (car items))
                                                     (list (deep-reverse (car items)))
                                                     (list (car items)))))))

(deep-reverse (list (list 1 2) (list 3 4)))
(deep-reverse (list (list 1 2) (list 3 4 (list 5 6))))

;;; 2.28

(define (fringe items)
  (cond ((null? items) null)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items)) (fringe (cdr items))))))

(fringe (list (list 1 2) (list 3 4)))
(fringe (list (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4))))

;;; 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a.
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;; b.
(define (total-weight mobile)
  (cond ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

;; c.
(define (balanced? mobile)
  (cond ((not (pair? mobile)) #t)
        ((eq? (* (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile))))
              (* (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile)))))
         (and (balanced? (branch-structure (left-branch mobile)))
              (balanced? (branch-structure (right-branch mobile)))))
        (else #f)))

(define a (make-mobile (make-branch 2 3) (make-branch 2 3))) 
(total-weight a)

(define d (make-mobile (make-branch 10 a) (make-branch 12 5))) 
  
(balanced? d)

(define m1 (make-mobile 
            (make-branch 4 6) 
            (make-branch 5 
                         (make-mobile 
                          (make-branch 3 7) 
                          (make-branch 9 8)))))
(total-weight m1)

(define m2 (make-mobile 
            (make-branch 4 6) 
            (make-branch 2 
                         (make-mobile 
                          (make-branch 5 8) 
                          (make-branch 10 4)))))
(balanced? m2)
(balanced? m1)

;; d.
;;Change cadr to cdr in (right-branch mobile) and (branch-structure branch)