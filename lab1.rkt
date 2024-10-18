
; 2 Using DrRacket
#lang typed/racket

(require typed/rackunit)

(+ 3 4)

(check-equal? (* 4 13) 52)

; 3 Elements of Racket
(check-equal? (or false true) #t)

; add two numbers together
(define (add-nums [a : Real] [b : Real]) : Real
  (+ a b))

; add-nums test-cases 
(check-equal? (add-nums 1 2) 3)
(check-equal? (add-nums 13804134 148109280948) 148123085082)
(check-equal? (add-nums 0 0) 0)


; 4 Exercises
; 4.1 Simple data

;; exercise 15 function definition
(define (==> [sunny : Boolean] [friday : Boolean]) : Boolean
  ;; if it's friday or not sunny, return true
  (or (not sunny) friday))
;; exercise 15 test cases
(check-equal? (==> #f #t) #t)
(check-equal? (==> #t #t) #t)
(check-equal? (==> #f #f) #t)
(check-equal? (==> #t #f) #f)

;; exercise 19 function definition
(define (string-insert [str : String] [i : Integer]) : String
  ;; append "_" at ith position of str
  (string-append  (substring str 0 i) "_" (substring str i)))

;; exercise 19 test cases
(check-equal? (string-insert "test" 2) "te_st")
(check-equal? (string-insert " " 0) "_ ")
(check-equal? (string-insert " " 1) " _")
(check-equal? (string-insert "testing" 7) "testing_")
(check-equal? (string-insert "piz za" 3) "piz_ za")

;; exercise 27
; constants
(define PEOPLE 120)
(define PRICE 5.0)
(define AVG 15)
(define CENT-CHANGE 0.1)
(define FIXED-COST 180)
(define VARIABLE-COST 0.04)

(define (thefunction [a : Real])
  (* a 5))
  
; attendees function definition
(define (attendees [ticket-price : Real]) : Real
  ; gets number of attendees
  (- PEOPLE (* (- ticket-price PRICE) (/ AVG CENT-CHANGE))))

; test cases
(check-= (attendees 3) 420 0.01)

; revenue function definition
(define (revenue [ticket-price : Real]) : Real
  ; returns revenue from attendees
  (* ticket-price (attendees ticket-price)))

; test cases
(check-= (revenue 3) 1260 0.01) 

; cost function definition
(define (cost [ticket-price : Real]) : Real
  ; gets the cost of ticket price
  (+ FIXED-COST (* VARIABLE-COST (attendees ticket-price)))) 

; test cases:
(check-equal? (cost 3) 196.8)

; profit function definition
(define (profit [ticket-price : Real]) : Real
  ; difference between cost and revenue
  (- (revenue ticket-price)
     (cost ticket-price)))

; exercise 27 test cases
(check-equal? (profit 1) 511.2)
(check-equal? (profit 2) 937.2)
(check-equal? (profit 3) 1063.2)

; 4.2 Intervals
(define (interest [deposit : Real]) : Real
  (cond [(<= deposit 1000)(* deposit 0.04)]
        [(<= deposit 5000)(* deposit 0.045)]
        [else (* deposit 0.05)]))

; test cases
(check-= (interest 1000) 40 0 "works")
(check-= (interest 1234) 55.53 0 "works")
(check-= (interest 0) 0 0 "works")
(check-= (interest 5432) 271.6 0 "works")

; 4.4 Structures
(define-type Furniture (U desk bookshelf))

(struct desk ([width : Real] [height : Real] [depth : Real]))
(struct bookshelf ([depth : Real] [shelves : Real] [width : Real]))

(: furniture-footprint (-> Furniture Real))
(define (furniture-footprint f)
  ; Calculates floor area
  (match f
    [(desk width _ depth) (* width depth)]
    [(bookshelf depth _ width) (* width depth)]))

; Test Cases for desk
(check-equal? (furniture-footprint (desk 5 4 3)) 15)
(check-equal? (furniture-footprint (desk 2 3 4)) 8)
(check-equal? (furniture-footprint (desk 125 100 150)) 18750)

; Test Cases for Bookshelf
(check-equal? (furniture-footprint (bookshelf 5 4 3)) 15)
(check-equal? (furniture-footprint (bookshelf 2 3 4)) 8)
(check-equal? (furniture-footprint (bookshelf 125 100 150)) 18750)

