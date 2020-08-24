#lang simply-scheme

;;; 3.17

(define (count-pairs-new x)
  (let ((seen '()))
    (define (local x)
      (if (memq x seen)
          0
          (begin (set! seen (cons x seen))
                 1)))
    (define (iter x)
      (if (not (pair? x))
          0
          (+ (local x)
             (iter (car x))
             (iter (cdr x)))))
    (iter x)))


(define x (list 1 2 3))
(count-pairs-new x)
(define listx (list x))
(count-pairs-new listx)
(define consx (cons x x))
(count-pairs-new consx)