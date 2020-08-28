#lang simply-scheme

;;; 3.23

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (print-deque deque)
  (let ((tmp (front-ptr deque)))
    (define (print-iter d)
      (if (null? d)
          '()
          (if (null? (cdr d))
              (cons (caar d) '())
              (cons (caar d) (print-iter (cdr d))))))
    (print-iter tmp)))

;;constructor
(define (make-deque) (cons '() '()))

;;predicate
(define (empty-deque? deque)
  (null? (front-ptr deque)))

;;selectors
(define (front-deque deque)
  (if (empty-deque? deque)
      (begin (display "FRONT called with an empty deque") (newline))
      (caar (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (begin (display "REAR called with an empty deque") (newline))
      (caar (rear-ptr deque))))

;;mutators
(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque))
          (else
           (set-cdr! new-pair (front-ptr deque))
           (set-cdr! (car (front-ptr deque)) new-pair)
           (set-front-ptr! deque new-pair)
           (print-deque deque)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (begin (display "FRONT-DELETE! called with an empty deque")) (newline))
        (else (set-front-ptr! deque (cdr (front-ptr deque)))
              (if (not (null? (front-ptr deque)))
                  (set-cdr! (car (front-ptr deque)) '())
                  (set-rear-ptr! deque '()))
              (print-deque deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque))
          (else
           (set-cdr! (car new-pair) (rear-ptr deque))
           (set-cdr! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque)))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (begin (display "REAR-DELETE! called with an empty deque")) (newline))
        (else (set-rear-ptr! deque (cdar (rear-ptr deque)))
              (if (not (null? (rear-ptr deque)))
                  (set-cdr! (rear-ptr deque) '())
                  (set-front-ptr! deque '()))
              (print-deque deque))))

;;testcase
(define d1 (make-deque))
(front-deque d1)
(rear-deque d1)
(front-insert-deque! d1 'a)
(front-insert-deque! d1 'b)
(front-insert-deque! d1 'c)
(front-delete-deque! d1)
(rear-delete-deque! d1)
(front-deque d1)
(rear-deque d1)
(rear-delete-deque! d1)
(front-delete-deque! d1)
(rear-insert-deque! d1 'i)
(front-insert-deque! d1 'j)
(rear-insert-deque! d1 'k)
(front-deque d1)
(rear-deque d1)
(rear-delete-deque! d1)
(front-delete-deque! d1)
(rear-delete-deque! d1)
(rear-delete-deque! d1)