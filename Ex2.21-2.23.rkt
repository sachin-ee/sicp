#lang simply-scheme

;;; 2.21

(define (square-list-1 items)
  (if (null? items)
      null
      (cons ((lambda (x) (* x x)) (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

(square-list-1 (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))

;;; 2.22

(define (square x) (* x x))

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

;; does not work bcos square of item is added to front of answer
(square-list-3 (list 1 2 3 4))

(define (square-list-4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

;; cons is good at adding element at front, not back.
;; Here answer is a list and element is added to end of list
(square-list-4 (list 1 2 3 4))

;; 2.23

(define (for-each proc items)
  (if (null? items)
      #t
      (let ((item items))  ;; we can use begin primitive statement as well
        (proc (car item))
        (for-each proc (cdr item)))))

(for-each (lambda (x)
            (display x)
            (newline))
          (list 1 2 3 4 5))