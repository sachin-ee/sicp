#lang simply-scheme

;;; 3.7

(define (make-account-pass balance pass)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (check p)
    (eq? p pass))
  (define (dispatch p m)
    (cond ((not (eq? p pass)) (lambda (x) (display "Incorrect password")))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'check) (check p))
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

(define peter-acc (make-account-pass 100 'open-sesame))
;((acc 'secret-password 'withdraw) 40)
;((acc 'some-other-password 'deposit) 50)
;(newline)

(define (make-joint acc orig-pass new-pass)
  (if (acc orig-pass 'check)
      (lambda (arg-pass mess)
        (if (eq? arg-pass new-pass)
            (acc orig-pass mess)
            (error "Incorrect password")))
      (error "Incorrect password")))

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 40)
((peter-acc 'open-sesame 'withdraw) 40)
((paul-acc 'rosebud 'withdraw) 20)

      
;;; 3.8

(define f
  (let ((init (- 1)))
    (lambda (x)
      (if (= init (- 1))
          (begin
            (set! init 0)
            x)
          0))))

;(+ (f 0) (f 1))
(+ (f 1) (f 0))