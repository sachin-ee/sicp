#lang simply-scheme

;;; 3.21

;; make-queue creates a pair of front-ptr and rear-ptr which is what is printed by the interpreter. front-ptr points
;; to queue items and rear-ptr points to last pair. So we simply need to print the car of queue. When queue is empty
;; rear ptr still points to last pair as per the queue implementation.
(define (print-queue queue)
  (car queue))

;;; 3.22

(define (front-ptr queue) (queue 'front-ptr))
(define (rear-ptr queue) (queue 'rear-ptr))
(define (set-front-ptr! queue item)
  ((queue 'set-front-ptr!) item))
(define (set-rear-ptr! queue item)
  ((queue 'set-rear-ptr!) item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (begin (display "FRONT called with an empty queue") (newline))
      (car (front-ptr queue))))
(define (print-queue queue)
  (front-ptr queue))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           (print-queue queue))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           (print-queue queue)))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (begin (display "DELETE! called with an empty queue")) (newline))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              (print-queue queue))))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))        
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!)
             (lambda (x) (set! front-ptr x)))
            ((eq? m 'set-rear-ptr!)
             (lambda (x) (set! rear-ptr x)))))
    dispatch))