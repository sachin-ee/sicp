#lang simply-scheme

;;; 2.40

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (accumulate append null (map (lambda (i)
                                 (map (lambda (j) (list i j))
                                      (enumerate-interval 1 (- i 1))))
                               (enumerate-interval 1 n))))

(unique-pairs 5)

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (square x) (* x x))

(define (prime? x)
  (define (prime-it n count)
    (if (= n 1)
        #f
        (if (> (square count) n)
            #t
            (if (= (remainder n count) 0)
                #f
                (prime-it n (+ count 1))))))
  (prime-it x 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(prime-sum-pairs 5)

(define (prime-sum-pairs-new n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs-new 5)

;;; 2.41

(define (make-triplet-sum triplet)
  (list (car triplet) (cadr triplet) (caddr triplet) (+ (car triplet) (cadr triplet) (caddr triplet))))

(define (unique-ordered-triplets s)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
                (map (lambda (k)
                       (list i j k))
                     (enumerate-interval 1 s)))
              (enumerate-interval 1 s)))
   (enumerate-interval 1 s)))

(define (ordered-triplets s)
  (define (eq-triplet-sum? triplet)
    (eq? (+ (car triplet) (cadr triplet) (caddr triplet)) s))
  (map make-triplet-sum
       (filter eq-triplet-sum? (unique-ordered-triplets s))))

(ordered-triplets 10)