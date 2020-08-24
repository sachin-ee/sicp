#lang simply-scheme

;;; 3.12
;; (define x (list 'a 'b))
;; (define y (list 'c 'd))
;; (define z (append x y))
;; (cdr x)
;; (b)

;; (define w (append! x y))
;; (cdr x)
;; (b c d)

;;; 3.13
;; This will create a cycle in z where the last null pointer will point to z itself causing infinite recursion.

;;; 3.14
;; v -> (a)
;; w - > (d c b a)
