#lang typed/racket

(require typed/rackunit)


; 3. Programming Exercises
; 1. Function rev-str-app
; 1. Data Definition
;; a List is either
;; - (cons value List), or
;; - `() 

; 2. Purpose statement & Header:
;; Combine all strings in list as one string

(define (rev-str-app [lst : (Listof String)]) : String
  ; 4. Template
  #; (match lst
       [(cons f r) (...)]
       ['() '()])
  (match lst
    ;5. Function
    [(cons str r) (string-append str (rev-str-app r))]
    [`() ""]))
#; ("ball" ("juice" ("frog" `())))

; 3. Tests:
(check-equal? (rev-str-app (cons "ball" (cons "juice" (cons "frog" `())))) "balljuicefrog")
(check-equal? (rev-str-app (cons "first" (cons "test" (cons "case" `())))) "firsttestcase")

; 2. Type of rev-str-app: (-> (Listof String) String)
; Type of '+': A lot of functions. This makes sense as '+' is a function, unlike other conventional programming languages as a binary operation. Racket must use different types of input and output.

; 3. Representation for bicycles
(define-type bicycle (U Trek Bianchi Gunnar))
(struct Trek ([num : Real]) #:transparent)
(struct Bianchi ([num : Real]) #:transparent)
(struct Gunnar ([num : Real]) #:transparent)

; 5. Function only-treks
(define (only-treks [lst : (Listof bicycle)]) : (Listof Trek)
  ; Returns a list of only treks
  (match lst
    [(cons f r) (cond[(Trek? f) (cons f (only-treks r))]
                     [else (only-treks r)])]
    ['() '()]))


; Tests
(check-equal? (only-treks (list (Trek 1) (Bianchi 2) (Trek 3) (Gunnar 4) (Trek 5)))
              (list (Trek 1) (Trek 3) (Trek 5)))
(check-equal? (only-treks '()) '())
(check-equal? (only-treks (list (Bianchi 1))) '())

; 6. Function only-bianchis
(define (only-bianchis [lst : (Listof bicycle)]) : (Listof Bianchi)
  ; Returns a list of only bianchis
  (match lst
    [(cons f r) (cond[(Bianchi? f) (cons f (only-bianchis r))]
                     [else (only-bianchis r)])]
    ['() '()]))

; Tests
(check-equal? (only-bianchis (list (Bianchi 1) (Trek 2) (Bianchi 3) (Gunnar 4) (Bianchi 5)))
              (list (Bianchi 1) (Bianchi 3) (Bianchi 5)))

(check-equal? (only-bianchis '()) '())
(check-equal? (only-bianchis (list (Trek 1))) '())


; 7. Function only-these
(define (only-these [lst : (Listof bicycle)] [fn : (-> bicycle Boolean)]) : (Listof bicycle)
  (match lst
    [(cons f r) (cond [(fn f) (cons f (only-these r fn))]
                      [else (only-these r fn)])]
    ['() '()]))

; Test cases
(check-equal? (only-these (list (Bianchi 4) (Bianchi 3)) Bianchi?)
              (list (Bianchi 4) (Bianchi 3)))
(check-equal? (only-these (list (Trek 4) (Bianchi 3) (Trek 2)) Trek?)
              (list (Trek 4) (Trek 2)))

; 8. Function my-append
(define (my-append [l1 : (Listof String)] [l2 : (Listof String)]) : (Listof String)
  ; Appends 2 lists of strings into 1 list
  (match l1
    [(cons f r) (cons f(my-append r l2))]
    ['() l2]))

; Tests
(check-equal? (my-append (list "a" "b" "c") (list "d" "e" "f")) (list "a" "b" "c" "d" "e" "f"))
(check-equal? (my-append '() '()) '())
(check-equal? (my-append (list " ") (list " ")) (list " " " "))

; 9. Function my-take
(define (my-take [lst : (Listof Real)] [n : Real]) : (Listof Real)
  ; Returns the first n elements of the list
  (match lst
    [(cons f r) (cond [(<= n 0) '()]
                [else (cons f (my-take r (- n 1)))])]
    ['() '()]))
  

; Tests
(check-equal? (my-take (list 1 2 3 4) 3) (list 1 2 3))
(check-equal? (my-take (list 1 2 3 4) 2) (list 1 2))
(check-equal? (my-take '() 3) '())
(check-equal? (my-take (list 1 2 3 4) 5) (list 1 2 3 4))


