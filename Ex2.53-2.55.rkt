#lang simply-scheme

;;; 2.53

;(a b c)
(list 'a 'b 'c)
;((george))
(list (list 'george))
;((y1 y2))   ;; quoted list and list primitive have same behaviour.
;Their cdr is a list whereas cdr of cons is the items itself.
(cdr '((x1 x2) (y1 y2)))
;(y1 y2)
(cadr '((x1 x2) (y1 y2)))
;#f
(pair? (car '(a short list)))
;#f
(memq 'red '((red shoes) (blue socks)))
;'(red shoes blue socks)
(memq 'red '(red shoes blue socks))

;;; 2.54

(define (equal? a b)
  (cond ((and (pair? a) (pair? b)) (and (equal? (car a) (car b))
                                        (equal? (cdr a) (cdr b))))
        ((and (symbol? a) (symbol? b)) (eq? a b))
        ((and (null? a) (null? b)) #t)
        (else #f)))

;(trace equal?)
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

;;; 2.55

;quote
(car ''abracadabra)
;(abracadabra)
(cdr ''abracadabra)