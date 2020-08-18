#lang simply-scheme

;;; 2.74

;; a.

(define (get-record division-personnel-file employee)
  ((get 'record division-personnel-file) employee))

;; b.

(define (get-salary division-personnel-file employee-record)
  ((get 'salary division-personnel-file) employee-record))

;; c.

(define (find-employee-record employee-name division-list)
  (cond ((null? division-list) #f)
        ((get-record (car division-personnel-file) employee-name)
         (get-record (car division-personnel-file) employee-name))
        (else (find-employee-record employee-name (cdr division-list)))))

;; d.

;; New company needs to install record and salary infomation using put procedure and new company as division name.