#lang simply-scheme

;;; 3.69

(define (triples S T U)
  (cons-stream (list (stream-car S) (stream-car T) (stream-car U))
               (interleave
                (interleave
                 (stream-map (lambda (x) (list (stream-car S) (stream-car T) x)) (stream-cdr U))
                 (triples S (stream-cdr T) (stream-cdr U)))
                (triples (stream-cdr S) (stream-cdr T) (stream-cdr U)))))

(define (pythagorean-triples list-triples)
  (stream-filter (lambda (x)
                   (= (+ (car x) (cadr x))
                      (caddr x)))
                 list-triples))