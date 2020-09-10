#lang simply-scheme

(define random-numbers
  (cons-stream
   random-init
   (stream-map rand-update random-numbers)))

(define rand
  (lambda (msg)
    (cond ((eq? msg 'generate)
           (cons-stream
            random-init
            (stream-map rand-update random-numbers)))
          ((eq? msg 'reset)
           (lambda (init)
             (cons-stream
              init
              (stream-map rand-update random-numbers)))))))


(rand 'generate)
((rand 'reset) 5)