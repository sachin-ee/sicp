#lang simply-scheme

;;; 3.1

(define (make-accumulator init)
  (let ((sum init))
    (lambda (val)
      (begin (set! sum (+ sum val))
             sum))))

(define A (make-accumulator 5))
(A 10)
(A 10)

;;; 3.2

(define (make-monitored f)
  (let ((count 0))
    (lambda (y)
      (define mf
        (cond ((equal? y 'how-many-calls?) count)
              ((equal? y 'reset-count) (set! count 0))
              (else (begin (set! count (+ count 1))
                           (f y)))))
      mf)))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 100)
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
(s 25)
(s 'how-many-calls?)

;;; 3.3

(define (make-account-pass balance pass)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p pass)) (lambda (x) (display "Incorrect password")))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account-pass 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
(newline)

;;; 3.4

(define (make-account-pass-cops balance pass)
  (let ((incorr-pass-cnt 0))
    (define (call-the-cops) (error "Calling Cops"))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch p m)
      (cond ((> incorr-pass-cnt 6)
             (call-the-cops))
            ((not (eq? p pass))
             (begin (set! incorr-pass-cnt (+ incorr-pass-cnt 1))
                    (lambda (x)
                      (begin
                        (display "Wrong password. Try again.")
                        (newline)))))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ))
    dispatch))

(define acc2 (make-account-pass-cops 100 'secret-password))
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)
((acc2 'some-other-password 'deposit) 50)