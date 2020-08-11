#lang simply-scheme

;;; 1.29

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (inc a) ( + a 1))
  (define (next-y k)
    (if (and (> k 0) (< k n))
        (if (even? k)
            (* 2 (y k))
            (* 4 (y k)))
        (y k)))
  (* (/ h 3) (sum next-y a inc n)))

(simpson cube 0 1 100.0)
(simpson cube 0 1 1000.0)

;;; 1.30

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-cubes-it a b)
  (sum-it cube a inc b))

(define (sum-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum-cubes 1 10)
(sum-cubes-it 1 10)

;;; 1.31

(define (product a b)
  (if (> a b)
      1
      (* a (product (+ a 1) b))))

(product 1 5)
(product 1 6)

;;; recursive
(define (product-h term a next b)
  (if (> a b)
      1
      (* (term a) (product-h term (next a) next b))))

(define (product-new a b)
  (product-h cube a inc b))

(product-new 1 3)

(define (identity x) x)

(define (factorial n)
  (product-h identity 1 inc n))

(factorial 5)
(factorial 6)
(factorial 7)

;;;;;

(define (inc2 x) (+ x 2))

(define (pi a b)
  (* 4.0 a b (/ (* (product-h identity (+ a 2) inc2 (- b 2))
                 (product-h identity (+ a 2) inc2 (- b 2)))
              (* (product-h identity (+ a 1) inc2 (- b 1))
                 (product-h identity (+ a 1) inc2 (- b 1))))))

(pi 2 500)

;;;

;;iterative
(define (product-it term a next b res)
  (if (> a b)
      res
      (product-it term (next a) next b (* (term a) res))))

(define (factorial-it n)
  (product-it identity 1 inc n 1))

(factorial-it 5)
(factorial-it 6)
(factorial-it 7)

;;; 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (new-sum a b)
  (accumulate + 0 identity a inc b))

(define (new-prod a b)
  (accumulate * 1 identity a inc b))

(new-sum 1 5)
(new-prod 1 5)

;;; 1.33

(define (square x) (* x x))

(define (prime? x b)
  (define (prime-it n count)
    (if (= n 1)
        #f
        (if (> (square count) n)
            #t
            (if (= (remainder n count) 0)
                #f
                (prime-it n (+ count 1))))))
  (prime-it x 2))

(prime? 1 0)
(prime? 2 0)
(prime? 3 0)
(prime? 4 0)
(prime? 5 0)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 256 76)
(gcd 2 10)

(define (gcd-prime? x b)
  (if (= (gcd x b) 1)
      #t
      #f))

(define (filtered-accumulate combiner null-value term a next b pred)      
  (if (> a b)
      null-value
      (if (pred a b)
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b pred))
          (filtered-accumulate combiner null-value term (next a) next b pred))))

(filtered-accumulate + 0 square 1 inc 5 prime?)

(filtered-accumulate * 1 identity 1 inc 10 gcd-prime?)

