#lang simply-scheme

;;; 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; 2.45

(define (split arg1 arg2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split arg1 arg2) painter (- n 1))))
          (arg1 painter (arg2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

;;; 2.46

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;;; 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cddr f))

;;; 2.48

(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;;; 2.49

;; a.

(define origin (make-vect 0 0))
(define edge1 (make-vect 1 0))
(define edge2 (make-vect 0 1))
(define edge3 (make-vect 1 1))

(define (frame-outline-painter frame)
  ((segments->painter (list (make-segment origin edge1)
                            (make-segment origin edge2)
                            (make-segment edge1 edge3)
                            (make-segment edge2 edge3)))
   frame))

;;b.
(define (X-painter frame)
  ((segments->painter (list (make-segment origin edge3)
                            (make-segment edge1 edge2)))
   frame))

;; c.
(define mid1 (make-vect 0.5 0))
(define mid2 (make-vect 1 0.5))
(define mid3 (make-vect 0.5 1))
(define mid4 (make-vect 0 0.5))

(define (diamond-painter frame)
  ((segments->painter (list (make-segment mid1 mid2)
                            (make-segment mid2 mid3)
                            (make-segment mid3 mid4)
                            (make-segment mid4 mid1)))
   frame))

;;; 2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;;; 2.51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-down
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
          (paint-up
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-down frame)
        (paint-up frame)))))

(define (below painter1 painter2)
  (lambda (frame)
    (let ((new-painter1 (rotate270 painter1))
          (new-painter2 (rotate270 painter2)))
      (rotate90 ((beside new-painter1 newpainter2) frame)))))

;;; 2.52

;; b.
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; c.
(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))