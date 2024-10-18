#lang racket

(require typed/rackunit)

; takes a number and returns a function to add 2 numbers
(define (curried-add a)
  (lambda (b) (+ a b)))

(define (curry2 f)
  (lambda (x)
    (lambda (y)
    (f x y))))


; ----------------------------------------------------------------------------------
; ----------------------------------- TEST CASES -----------------------------------
; ----------------------------------------------------------------------------------

; curried-add
(check-equal? ((curried-add 5) 3) 8) ; 5 + 3 = 8
(check-equal? ((curried-add 0) 10) 10) ; 0 + 10 = 10
(check-equal? ((curried-add -4) 6) 2)  ; -4 + 6 = 2
(check-equal? ((curried-add 7) -3) 4)  ; 7 + (-3) = 4

; curry2
(define add (lambda (x y) (+ x y)))

(define curried-addy (curry2 add))

(define add-five (curried-addy 5))
(check-equal? (add-five 3) 8)

