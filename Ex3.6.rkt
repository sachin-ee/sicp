#lang simply-scheme

;;; 3.6

(define random-init 0)

(define (rand-update x)
  (+ x 1))

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin
               (set! x (rand-update x))
               x))
            ((eq? m 'reset)
             (lambda (y)
               (begin
                 (set! x y)
                 x)))))))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 10)
(rand 'generate)
(rand 'generate)
(rand 'generate)