#lang simply-scheme

;;; 2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (if (not (null? x))
                                (+ 1 y)
                                0)) 0 sequence))


(define (square x) (* x x))
(map square (list -1 0 5))
(map (lambda (x) (* x x)) (list 1 2 3))
(append (list 1 2) (list 3 4))
(length (list 1 2 3 4 5 6))

;;; 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;;; 2.35

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))

(define (count-leaves-acc tree)
  (accumulate + 0 (map (lambda(x) (if (not (pair? x))
                                      1
                                      (+ (count-leaves-acc (car tree)))))
                       tree)))

(define x (cons (list 1 2) (list 3 4 (list 5 6))))
(count-leaves x)
(count-leaves (list x x x))

(count-leaves-acc x)
(count-leaves-acc (list x x x))