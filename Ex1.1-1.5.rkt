#lang simply-scheme

;;;1.3

(define (sum-of-squares-of-larger-two x y z)
  (cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
        ((and (< y x) (< y z)) (+ (* x x) (* z z)))
        ((and (< z x) (< z y)) (+ (* x x) (* y y)))))

;;; 1.5
;;Applicative order - infinite recursion
;;Normal order- lazy evaluation - returns 0