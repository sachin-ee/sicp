#lang simply-scheme

;;; 3.25

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup-new keys table)
  (let ((subtable
         (assoc (car keys) (cdr table))))
    (if subtable
        (if (not (null? (cdr keys)))
            (lookup-new (cdr keys) subtable)
            (cdr subtable))
        false)))

(define (insert-new! keys value table)
  (let ((subtable (assoc (car keys) (cdr table))))
    (if subtable
        (if (not (null? (cdr keys)))
            (insert-new! (cdr keys) value subtable)
            (set-cdr! subtable value))
        (begin (set-cdr! table (cons (cons (car keys) '()) (cdr table)))
               (insert-new! keys value table))))
  'ok)

(define (make-table)
  (list '*table*))

;;testcase
(define t1 (make-table))
(lookup-new (list 'a) t1)
(lookup-new (list 'b) t1)
(lookup-new (list 'c) t1)
(insert-new! (list 'a) 1 t1)
(insert-new! (list 'b) 2 t1)
(insert-new! (list 'c) 3 t1)
(lookup-new (list 'a) t1)
(lookup-new (list 'b) t1)
(lookup-new (list 'c) t1)
(lookup-new (list 'math '+) t1)
(lookup-new (list 'math '-) t1)
(lookup-new (list 'math '*) t1)
(lookup-new (list 'letters 'a) t1)
(lookup-new (list 'letters 'b) t1)
(insert-new! (list 'math '+) 43 t1)
(insert-new! (list 'math '-) 45 t1)
(insert-new! (list 'math '*) 42 t1)
(insert-new! (list 'letters 'a) 97 t1)
(insert-new! (list 'letters 'b) 98 t1)
(lookup-new (list 'math '+) t1)
(lookup-new (list 'math '-) t1)
(lookup-new (list 'math '*) t1)
(lookup-new (list 'letters 'a) t1)
(lookup-new (list 'letters 'b) t1)
(lookup-new (list 'letters 'c) t1)