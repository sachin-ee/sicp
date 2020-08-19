#lang simply-scheme

;;;; 2.75

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;; 2.76

;;; Data-directed most appropriate for adding new operations often since it requires only adding the new operation to a table
;;; Message-passing style is appropriate for adding new types often since it dispatches on existing operations and defining just the new type