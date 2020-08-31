#lang simply-scheme

;;; 3.38

;; a.
; 40 - Mary, Peter, Paul
; 35 - Peter, Mary, Paul
; 50 - Paul, Mary, Peter
; 45 - Peter, Paul, Mary

;;; 3.39

; 101: P1 sets x to 100 and then P2 increments x to 101.
; 121: P2 increments x to 11 and then P1 sets x to x * x.
; 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.

;;; 3.40

;; a.
; 1000000: P1 followed by P2
; 1000000: P2 followed by P1
; 100    : P1 computes 100, P2 writes 1000 to x, P1 sets 100 to x
; 1000   : P2 computes 1000, P1 writes 100 to x, P2 sets 1000 to x
; 10000  : P2 computes between x in P1
; 100000 : P1 computes between first 2 x's in P2
; 10000  : P1 computes between second 2 x's in P2

;; b.
; 1000000: P1 followed by P2
; 1000000: P2 followed by P1

;;; 3.41

; No need to protect balance since when balance is accessed and either withdraw or deposit is being executed
; either we get previous balance or new balance and there is no intermediate results. Both are valid.

;;; 3.42

; Its safe to use the new version and there are no differences in concurrency