#lang simply-scheme

;;; 3.48

(define false #f)
(define true #t)

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))


(define (make-account-and-serializer-unique)
  (let ((counter 0))
    (define (new-dispatch m b)
      (define (make-account-and-serializer balance)
        (define (withdraw amount)
          (if (>= balance amount)
              (begin (set! balance (- balance amount))
                     balance)
              "Insufficient funds"))
        (define (deposit amount)
          (set! balance (+ balance amount))
          balance)
        (let ((balance-serializer (make-serializer)))
          (define (dispatch m)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'balance) balance)
                  ((eq? m 'serializer) balance-serializer)
                  ((eq? m 'number) (begin (set! counter (+ counter 1))
                                          counter))
                  (else (error "Unknown request: MAKE-ACCOUNT" m))))
          dispatch))
      (cond ((eq? m 'new-account) (make-account-and-serializer b))
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    new-dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (new-serialized-exchange account1 account2)
  (let ((uniq1 (account1 'number))
        (uniq2 (account2 'number))
        (serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< uniq2 uniq1)
        ((serializer1 (serializer2 exchange)) account1 account2)
        ((serializer2 (serializer1 exchange)) account1 account2))))

;; testcase
(define make-account (make-account-and-serializer-unique))
(define acc1 (make-account 'new-account 100))
(define acc2 (make-account 'new-account 100))
((acc1 'deposit) 10)
((acc2 'withdraw) 10)
(acc1 'balance)
(acc2 'balance)
(new-serialized-exchange acc1 acc2)
(new-serialized-exchange acc2 acc1)

;; This will avoid deadlock since a process will try to acquire lock on lowest numbered procedure always first
;; followed by next lowest and so on.