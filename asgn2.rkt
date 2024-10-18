#lang typed/racket

(require typed/rackunit)

; Exercise 2.3.3.
(define CUSTOMER 5)
(define PERFORMANCE 20)
(define RATE 0.5)

; Total-profit subtracting total expenses from total-income
(define (total-profit [attendees : Real]) : Real
  (- (* attendees CUSTOMER) (+ (* attendees RATE) PERFORMANCE)))

; Exercise 3.3.3.
; Returns Surface Area of a cylinder given base radius and height
(define (area-cylinder [r : Real] [h : Real]) : Real
  (+ (* h r 2 pi) (* 2 r r pi)))

; 2.2 Fruits
; represents a food
(define-type Food (U Blueberry Bread Banana))
 
; Represents a blueberry with blueness in blue-units and count amount of taste
(struct Blueberry ([blueness : Nonnegative-Real] [count : Nonnegative-Integer]) #:transparent)
; Represents bread with hydration ratio
(struct Bread ([hydration : Nonnegative-Real]) #:transparent)
; Represents a banana with grams weight
(struct Banana ([grams : Nonnegative-Real]) #:transparent)

; Returns if food is delicious based on certain attributes
(define (delicious? [f : Food]) : Boolean
  (match f
    [(Blueberry blueness count) (and (> blueness 5) (>= count 100))]
    [(Bread hydration) (and (>= hydration 0.7) (<= hydration 0.9))]
    [(Banana _) #t]))

; 2.3 Low-degree Polynomials
(define-type Polynomial (U Linear Quadratic))
(struct Linear ([A : Real] [B : Real]) #:transparent) ; Ax + B
(struct Quadratic ([A : Real] [B : Real] [C : Real]) #:transparent) ; Ax^2 + Bx + C

; Given a polynomial and value, return their value for their respective function
(define (interp [poly : Polynomial] [x : Real]) : Real
  (match poly
    [(Linear a b) (+ (* a x) b)]
    [(Quadratic a b c) (+ (* a(* x x)) (* b x) c)]))

; 2.4 Derivative
; Given a polynomial, return its derivative
(define (derivative [poly : Polynomial]) : Polynomial
  (match poly
    [(Linear a _) (Linear 0 a)]
    [(Quadratic a b _) (Linear (* a 2) b)])) 

; 2.5 Binary Tree
;; A BTree is either:
;; (Leaf) 
;; (Node symbol BTree BTree)
(define-type BTree (U Leaf Node))
(struct Node ([x : Symbol] [left : BTree] [right : BTree]) #:transparent)
(struct Leaf () #:transparent)

; Examples
(define example-BTree-1 (Node 'A (Node 'B (Leaf) (Leaf)) (Leaf)))
(define example-BTree-2 (Node 'A (Leaf) (Node 'B (Node 'C (Leaf) (Leaf)) (Leaf))))
(define example-BTRee-3 (Leaf))

; 2.6 Mirror
; Given a BTree, return a flipped/mirrored version of the BTree
(define (mirror [bt : BTree]) : BTree
  (match bt
    [(Node sym l r) (Node sym (mirror r) (mirror l))]
    [(Leaf) (Leaf)]))

; 2.7 Left-Spine
; Given a BTree, return only the left side of tree.
(define (left-spine [bt : BTree]) : BTree
  (match bt
    [(Node sym l _) (Node sym (left-spine l) (Leaf))]
    [(Leaf) (Leaf)]))

; 2.8 Containment
; Return True if given symbol is in BTree, otherwise False.
(define (contains? [bt : BTree] [find : Symbol]) : Boolean
  (match bt
    [(Node sym l r) (cond [(equal? sym find) #t]
                          [else (or (contains? l find) (contains? r find))])]
    [(Leaf) #f]))

; 2.9 Occurences
; Return the number of occurences of the symbol given a BTree. 
(define (occurrences [bt : BTree] [find : Symbol]) : Real
  (match bt
    [(Node sym l r) (cond [(equal? sym find) (+ 1 (+ (occurrences l find) (occurrences r find)))]
                          [else (+ (occurrences l find) (occurrences r find))])]
    [(Leaf) 0]))

; 2.10 Substitution
; Replace all nodes of the same symbol if the symbol can be found in BTree,
; otherwise no change
(define (subst [bt : BTree] [find : Symbol] [replace : BTree]) : BTree
  (match bt
    [(Node sym l r) (cond [(equal? sym find) replace]
                          [else (Node sym (subst l find replace) (subst r find replace))])]
    [(Leaf) (Leaf)]))

; 2.11 All Path Lengths
; calls find-path
(define (all-path-lengths [bt : BTree]) : (Listof Real)
  (find-path bt 0))

; Creates a list of lengths from root to every leaf in BTree
(define (find-path [bt : BTree] [cur-len : Real]) : (Listof Real)
  (match bt
    [(Node _ l r) (append (find-path l (+ cur-len 1)) (find-path r (+ cur-len 1)))]
    [(Leaf) (list cur-len)]))

; -----------------------------------------------------------------
; ---------------------------- TEST CASES -------------------------
; -----------------------------------------------------------------

; total-profit
(check-equal? (total-profit 5) 2.5)
(check-equal? (total-profit 0) -20)
(check-= (total-profit 10) 25 0.01)
(check-= (total-profit 15) 47.5 0.01)

; area-cylinder
(check-= (area-cylinder 3 3) 113.1 0.1)
(check-= (area-cylinder 0 0) 0 0)
(check-= (area-cylinder 5 3) 251.33 0.1)
(check-= (area-cylinder 3 2) 94.25 0.1)

; delicious?
(check-equal? (delicious? (Blueberry 6 100)) #t)
(check-equal? (delicious? (Blueberry 6 1)) #f)
(check-equal? (delicious? (Blueberry 5 100)) #f)
(check-equal? (delicious? (Blueberry 10 999)) #t)
(check-equal? (delicious? (Blueberry 0 0)) #f)

(check-equal? (delicious? (Bread 0.7)) #t)
(check-equal? (delicious? (Bread 0.8)) #t)
(check-equal? (delicious? (Bread 0.9)) #t)
(check-equal? (delicious? (Bread 0)) #f)
(check-equal? (delicious? (Bread 1)) #f)
(check-equal? (delicious? (Bread 0.7894)) #t)
(check-equal? (delicious? (Bread 0.901)) #f)
(check-equal? (delicious? (Bread 0.699)) #f)

(check-equal? (delicious? (Banana 9)) #t)
(check-equal? (delicious? (Banana 0)) #t)
(check-equal? (delicious? (Banana 82.9)) #t)

; interp
(check-= (interp (Linear 3 5) 3) 14 0)
(check-= (interp (Linear 0 0) 3) 0 0)
(check-= (interp (Linear 3.2 7.1) 23.1) 81.02 0.1)

(check-= (interp (Quadratic 3 5 7) 3) 49 0.1)
(check-= (interp (Quadratic 0 0 0) 1) 0 0)
(check-= (interp (Quadratic 3.2 7.1 8.913) 23.1) 1880.475 0.1)

; derivative
(check-equal? (derivative (Linear 3 7)) (Linear 0 3)) ; 3x + 7 dx/dy = 0x + 3
(check-equal? (derivative (Linear 0 0)) (Linear 0 0)) ; 0x + 0 dx/dy = 0
(check-equal? (derivative (Linear 0 931)) (Linear 0 0)) ; 931 dx/dy = 0
(check-equal? (derivative (Linear 319 9041)) (Linear 0 319)) ; 319x + 9041 dx/dy = 319

(check-equal? (derivative (Quadratic 3 7 5)) (Linear 6 7)) ; 3x^2 + 7x + 5 dx/dy = 6x + 7
(check-equal? (derivative (Quadratic 0 0 0)) (Linear 0 0)) ; 0x^2 + 0x + 0 dx/dy = 0 + 0
(check-equal? (derivative (Quadratic 7 2 1)) (Linear 14 2)) ; 7x^2 + 2x + 1 dx/dy = 14x + 2

; mirror
(check-equal? (mirror (Node 'A (Node 'B (Node 'horse (Leaf) (Leaf)) (Leaf)) (Leaf)))
              (Node 'A (Leaf) (Node 'B (Leaf) (Node 'horse (Leaf) (Leaf)))))
(check-equal? (mirror (Leaf)) (Leaf))
(check-equal? (mirror (Node 'A (Leaf) (Leaf)))
              (Node 'A (Leaf) (Leaf)))
(check-equal? (mirror (Node 'A (Leaf) (Node 'B (Leaf) (Leaf))))
              (Node 'A (Node 'B (Leaf) (Leaf)) (Leaf)))

; left-spine
(check-equal? (left-spine (Node 'A (Node 'B (Node 'C (Leaf) (Node 'nothing (Leaf) (Leaf)))
                                         (Leaf)) (Node 'nothing (Leaf) (Leaf))))
              (Node 'A (Node 'B (Node 'C (Leaf) (Leaf)) (Leaf)) (Leaf)))
(check-equal? (left-spine (Leaf)) (Leaf))
(check-equal? (left-spine (Node 'A (Leaf) (Node 'B (Leaf) (Node 'C (Leaf) (Leaf)))))
              (Node 'A (Leaf) (Leaf)))

; contains?
(check-equal? (contains? (Node 'A (Leaf) (Leaf)) 'A) #t)
(check-equal? (contains? (Node 'A (Leaf) (Leaf)) 'B) #f)
(check-equal? (contains? (Node 'B (Leaf) (Leaf)) 'A) #f)
(check-equal? (contains? (Node 'A (Node 'B (Leaf) (Leaf)) (Leaf)) 'A) #t)
(check-equal? (contains? (Node 'A (Node 'B (Leaf) (Leaf)) (Leaf)) 'B) #t)
(check-equal? (contains? (Node 'A (Node 'B (Leaf) (Leaf)) (Leaf)) 'C) #f)
(check-equal? (contains? (Node 'A (Leaf) (Node 'B (Leaf) (Leaf))) 'B) #t)

; occurrences
(check-equal? (occurrences (Node 'A (Node 'B (Node 'C (Leaf) (Node 'nothing (Leaf) (Leaf)))
                                          (Leaf)) (Node 'nothing (Leaf) (Leaf))) 'nothing)
              2)
(check-equal? (occurrences (Node 'A (Node 'A (Node 'A (Leaf) (Node 'A (Leaf) (Leaf)))
                                          (Leaf)) (Node 'A (Leaf) (Leaf))) 'A)
              5)
(check-equal? (occurrences (Node 'A (Leaf) (Leaf)) 'A) 1)
(check-equal? (occurrences (Node 'A (Node 'A (Node 'C (Leaf) (Node 'nothing (Leaf) (Leaf)))
                                          (Leaf)) (Node 'nothing (Leaf) (Leaf))) 'B)
              0)

; subst
(check-equal? (subst (Node 'A (Leaf) (Node 'B (Leaf) (Leaf))) 'B (Leaf))
              (Node 'A (Leaf) (Leaf)))
(check-equal? (subst (Node 'A (Node 'B (Leaf) (Leaf)) (Node 'C (Leaf) (Leaf))) 'B
                     (Node 'D (Leaf) (Node 'E (Leaf) (Leaf))))
              (Node 'A (Node 'D (Leaf) (Node 'E (Leaf) (Leaf))) (Node 'C (Leaf) (Leaf))))
(check-equal? (subst (Node 'A (Node 'B (Leaf) (Leaf)) (Node 'C (Leaf) (Leaf))) 'C
                     (Node 'D (Leaf) (Node 'E (Leaf) (Leaf))))
              (Node 'A (Node 'B (Leaf) (Leaf)) (Node 'D (Leaf) (Node 'E (Leaf) (Leaf)))))


; all-path-lengths
(check-equal? (all-path-lengths (Node 'A (Leaf) (Leaf)))
              (list 1 1))
(check-equal? (all-path-lengths (Leaf)) (list 0))
(check-equal? (all-path-lengths (Node 'A (Node 'B (Leaf) (Leaf)) (Leaf)))
              (list 2 2 1))
(check-equal? (all-path-lengths (Node 'A (Node 'B (Leaf) (Leaf))
                                      (Node 'D (Leaf) (Node 'E (Leaf) (Leaf)))))
              (list 2 2 2 3 3))