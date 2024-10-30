#lang racket

(require typed/rackunit)

; takes a number and returns a function to add 2 numbers
(define (curried-add a)
  (lambda (b) (+ a b)))

; takes a function of 2 arguments and runs it
(define (curry2 f)
  (lambda (a) 
    (lambda (b)
      (f a b))))

; takes a function of 3 arguments and runs it
(define (curry3 f)
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (f a b c)))))

; returns true if the symbol is in the list
(define (contains? lst sym)
  (match lst
    ['() #f]
    [(cons f r) (cond [(equal? f sym) #t]
                      [else (contains? r sym)])]))

; given 2 lists, return a list of booleans when they match
(define (in-list-many? lst-sym lst-query)
  (map ((curry2 contains?) lst-sym) lst-query))

; ----------------------------------------------------------------------------------
; ----------------------------------- TEST CASES -----------------------------------
; ----------------------------------------------------------------------------------

; curried-add
(check-equal? ((curried-add 5) 3) 8) ; 5 + 3 = 8
(check-equal? ((curried-add 0) 10) 10) ; 0 + 10 = 10
(check-equal? ((curried-add -4) 6) 2)  ; -4 + 6 = 2
(check-equal? ((curried-add 7) -3) 4)  ; 7 + (-3) = 4

; curry2
(check-equal? (((curry2 +) 1) 2) 3)
(check-equal? (((curry2 -) 3) 2) 1)
(check-equal? (((curry2 /) 12) 4) 3)


; curry3
(check-equal? ((((curry3 +) 1) 2) 3) 6)
(check-equal? ((((curry3 -) 6) 3) 2) 1)
(check-equal? ((((curry3 *) 1) 2) 3) 6)

; contains?
(check-equal? (contains? (list 1 2 3 4 5) 4) #t)
(check-equal? (contains? (list 'a 'b 'c 'd) 'd) #t)
(check-equal? (contains? (list 'a 'b 'c 'd) 'e) #f)

; in-list-many?
(check-equal? (in-list-many? (list 1 2 3 4) (list 2 2 3 4 5 6 7 1 2 3)) (list #t #t #t #t #f #f #f #t #t #t))
(check-equal? (in-list-many? (list 1 2 3 4) (list 5 6 7 8)) (list #f #f #f #f))
(check-equal? (in-list-many? (list 1 2 3 4) (list 1 6 7 4)) (list #t #f #f #t))
(check-equal? (in-list-many? (list 1 2 3 4) (list 1 2 7 4)) (list #t #t #f #t))
(check-equal? (in-list-many? (list 1 2 3 4) (list 1 2 7 4 6 7 8 9 0 11)) (list #t #t #f #t #f #f #f #f #f #f))
