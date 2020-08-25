#lang simply-scheme

;;; 3.18

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (cycle? x)
  (if (null? x)
      #f
      (let ((first-item x))
        (define (iter x)      
          (if (null? x)
              #f
              (if (eq? first-item (cdr x))
                  #t
                  (iter (cdr x)))))
        (iter x))))