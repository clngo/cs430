#lang typed/racket

(require typed/rackunit)

;; a List is either
;; - (cons value List), or
;; - `()

;; **cons is another name for Node

(define lst (cons 10 (cons 20 (cons 30 `()))))


;; `(((6)) 5) with length 2
(define my-cool-lst
  (cons (cons (cons 6 `()) `())
        (cons 5 `())))

; function takes in lst of nums, ret their sum

; 1. Data Definition
;

; 2. Purpose Statement and Header
; Sum up the numbers in a list
(define (sum-list [lst : (Listof Real)]) : Real
  ; 4. Template - You only care about input
  (match lst
    ; 5. Write the rest of the function
    [(cons f r) (+ f (sum-list r))]
    [`() 0]))

; 3. Tests
(check-equal? (sum-list (cons 1 (cons 2 (cons 3 (cons 4 `()))))) 10)
(check-equal? (sum-list `()) 0)
(check-equal? (sum-list (cons 1 (cons 1 (cons 2 (cons 3 (cons 5 `())))))) 12)
(check-equal? (sum-list (list 1 1 2 3 5)) 12)


