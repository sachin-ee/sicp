#lang simply-scheme

;;; 1.11

(define (func-r n)
  (cond ((< n 3) n)
        (else (+ (func-r (- n 1)) (* 2 (func-r (- n 2))) (* 3 (func-r (- n 3)))))))

(define (func-i n)
  (define (func-iter a b c count)
    (cond ((= count 0) a)
          (else (func-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (cond ((< n 3) n)
        (else (func-iter 2 1 0 (- n 2)))))

;;; 1.12

(define (pascal r c)
  (if (or (= r 1) (= c 1) (= c r))
      1
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))