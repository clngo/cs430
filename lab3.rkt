#lang typed/racket

(require typed/rackunit)

; 2. Exercises

; returns true if sexp has real, 'chris, and sym
(: parse000 (-> Sexp Boolean))
(define (parse000 sexp)
  (match sexp
    [(list (? real?) 'chris (? symbol?)) #t]
    [other #f]))

; returns symbol if sexp has real, 'chris, and sym
(: parse001 (-> Sexp (U Symbol Boolean)))
(define (parse001 sexp)
  (match sexp
    [(list (? real?) 'chris (? symbol? sym)) sym]
    [other #f]))

; returns second element of listof real if len is 3
(: parse002 (-> Sexp (U (Listof Real) Boolean)))
(define (parse002 sexp)
  (match sexp
    [(list ignore1 (list (? real? lst) ...) ignore2) (cast lst (Listof Real))]
    [other #f]))

; returns 'okay if given val is a number, otherwise error
(define (ohno [val : Any]) : Symbol
  (match val
    [(? real?) 'okay]
    [other (error 'ohno "expected a number, got ~e" val)]))

; Arith language
(define-type ArithC (U NumC PlusC MultC SquareC))
(struct NumC ([n : Real]) #:transparent )
(struct PlusC ([l : ArithC] [r : ArithC]) #:transparent)
(struct MultC ([l : ArithC] [r : ArithC]) #:transparent)
(struct SquareC ([n : ArithC]) #:transparent)

; evaluation method: calculates arithmetic expressions to numbers
(define (interp [a : ArithC]) : Real
  (match a
    [(NumC n) n]
    [(PlusC l r) (+ (interp l) (interp r))]
    [(MultC l r) (* (interp l) (interp r))]
    [(SquareC n) (* (interp n) (interp n))]))

; given a PlusC, swap the left and right terms
(define (swap-adds [a : ArithC]) : ArithC
  (printf "Entering swap-adds with tree: ~a\n" a)
  (match a
    [(PlusC l r) (PlusC (swap-adds r) (swap-adds l))]
    [(MultC l r) (MultC (swap-adds l) (swap-adds r))]
    [(NumC n) (NumC n)]))

; Given an Sexp, return the ArithC
(define (parse [s : Sexp]) : ArithC
  (match s
    [(? real? n) (NumC n)]
    [(list '+ l r) (PlusC (parse (second s)) (parse (third s)))]
    [(list '* l r) (MultC (parse (second s)) (parse (third s)))]
    [(list '^2 n) (SquareC (parse (second s)))]
    [other (error 'parse "invalid input: ~e" s)]))

; calls parse into interp function
(define (top-interp [s : Sexp]) : Real
  (interp (parse s)))

; List of lists containing corresponding elements of each 2 different lists
(define (zip [lst1 : (Listof Real)] [lst2 : (Listof Real)]) : (Listof (Listof Real))
  (match* (lst1 lst2)
    [((list) (list)) '()] 
    [((cons f1 r1) (cons f2 r2)) 
     (cons (list f1 f2) (zip r1 r2))]))

; ----------------------------------------------------------------------------------
; ----------------------------------- TEST CASES -----------------------------------
; ----------------------------------------------------------------------------------

; parse000
(check-equal? (parse000 (list 'chris)) #f)                    
(check-equal? (parse000 (list 1 'chris 'sym)) #t)              
(check-equal? (parse000 (list 2 'chris 32)) #f)                
(check-equal? (parse000 (list 3 'smth 'smth)) #f)              
(check-equal? (parse000 (list 'smth 'chris 'smth)) #f)         
(check-equal? (parse000 (list 432 'chris 'sdf)) #t)

; parse001
(check-equal? (parse001 (list 'chris)) #f)                    
(check-equal? (parse001 (list 1 'chris 'sym)) 'sym)              
(check-equal? (parse001 (list 2 'chris 32)) #f)                
(check-equal? (parse001 (list 3 'smth 'smth)) #f)              
(check-equal? (parse001 (list 'smth 'chris 'smth)) #f)         
(check-equal? (parse001 (list 432 'chris 'sdf)) 'sdf)

; parse002
(check-equal? (parse002 (list 'sdf (list 1 2 3 4) 'sdf)) (list 1 2 3 4))
(check-equal? (parse002 (list 132 'sdf 134)) #f)
(check-equal? (parse002 (list 'foo (list 1.1 2.2 3.3) 'bar)) (list 1.1 2.2 3.3))  
(check-equal? (parse002 (list 'foo (list 42) 'bar)) (list 42)) 
(check-equal? (parse002 (list 'foo (list 1 'not-a-number) 'bar)) #f) 
(check-equal? (parse002 (list 'foo 42 'bar)) #f)                  
(check-equal? (parse002 (list 'foo (list) 'bar)) '()) 
(check-equal? (parse002 (list 'foo 'not-a-list 'bar)) #f)     
(check-equal? (parse002 (list 'foo (list 3.14 1.618) 'baz)) (list 3.14 1.618))

; ohno
(check-equal? (ohno 13) 'okay)
(check-equal? (ohno 1039) 'okay)
(check-exn (regexp (regexp-quote "expected a number, got 'test"))
           (lambda () (ohno 'test)))
(check-exn (regexp (regexp-quote "expected a number, got 'alsdkfjalksdfj"))
           (lambda () (ohno 'alsdkfjalksdfj)))

; interp
(check-equal? (interp (PlusC (NumC 5) (NumC 0))) 5)
(check-equal? (interp (PlusC (NumC 5) (NumC 10))) 15)
(check-equal? (interp (PlusC (PlusC (NumC 5) (NumC 0)) (NumC 3))) 8)
(check-equal? (interp (PlusC (NumC 3) (PlusC (NumC 5) (NumC 0)))) 8)
(check-equal? (interp (MultC (NumC 5) (NumC 3))) 15)
(check-equal? (interp (MultC (NumC 0) (NumC 9103413840310))) 0)
(check-equal? (interp (MultC (PlusC (NumC 2) (NumC 5)) (MultC (NumC 3) (NumC 1)))) 21)

; swap-adds
(check-equal? (swap-adds (PlusC (NumC 1) (NumC 2))) (PlusC (NumC 2) (NumC 1)))
(check-equal? (swap-adds (PlusC (NumC 0) (NumC 0))) (PlusC (NumC 0) (NumC 0)))
(check-equal? (swap-adds (PlusC (NumC 2438) (NumC 13324))) (PlusC (NumC 13324) (NumC 2438)))
(check-equal? (swap-adds (PlusC (PlusC (NumC 193) (NumC 109348)) (NumC 931))) (PlusC (NumC 931) (PlusC (NumC 109348) (NumC 193))))
(check-equal? (swap-adds (PlusC (PlusC (NumC 193) (NumC 109348)) (MultC (NumC 31) (NumC 0)))) (PlusC (MultC (NumC 31) (NumC 0)) (PlusC (NumC 109348) (NumC 193))))
(check-equal? (swap-adds (MultC (PlusC (NumC 1) (NumC 2)) (NumC 3))) (MultC (PlusC (NumC 2) (NumC 1)) (NumC 3)))

; parse
(check-equal? (parse '1) (NumC 1))
(check-equal? (parse '(+ 1 2)) (PlusC (NumC 1) (NumC 2)))
(check-equal? (parse '(* 1 2)) (MultC (NumC 1) (NumC 2)))
(check-equal? (parse '(+ (* 1 2) (+ 2 3)))
              (PlusC (MultC (NumC 1) (NumC 2))
                     (PlusC (NumC 2) (NumC 3))))
(check-exn (regexp (regexp-quote "invalid input: 'test"))
           (lambda () (parse 'test)))
(check-exn (regexp (regexp-quote "invalid input: '(1 2)"))
           (lambda () (parse '(1 2))))
(check-exn (regexp (regexp-quote "invalid input: '+"))
           (lambda () (parse '+)))
(check-exn (regexp (regexp-quote "invalid input: '(1 + 2)"))
           (lambda () (parse '(1 + 2))))

; ^2 feature
(check-equal? (parse '(^2 3)) (SquareC (NumC 3)))
(check-equal? (parse '(+ (^2 2) 1)) (PlusC (SquareC (NumC 2)) (NumC 1)))
(check-equal? (parse '(+ (^2 (+ 2 1)) 1)) (PlusC (SquareC (PlusC (NumC 2) (NumC 1))) (NumC 1)))
(check-equal? (parse '(+ (^2 (+ 2 1)) (^2 (* 3 1))))
              (PlusC (SquareC (PlusC (NumC 2) (NumC 1)))
                     (SquareC (MultC (NumC 3) (NumC 1)))))

; top-interp
(check-equal? (top-interp '(^2 3)) 9)
(check-equal? (top-interp '(+ (^2 2) 1)) 5)
(check-equal? (top-interp '(+ (^2 (+ 2 1)) 1)) 10)
(check-equal? (top-interp '(+ (^2 (+ 2 1)) (^2 (* 3 1)))) 18)
(check-equal? (top-interp '(+ (* 1 2) (+ 2 3))) 7)
(check-exn (regexp (regexp-quote "invalid input: '(1 + 2)"))
           (lambda () (top-interp '(1 + 2))))
(check-exn (regexp (regexp-quote "invalid input: '(1 2)"))
           (lambda () (top-interp '(1 2))))

; zip
(check-equal? (zip (list 1 2 3) (list 4 5 6)) '((1 4) (2 5) (3 6)))
(check-equal? (zip (list 0) (list 0)) '((0 0)))
(check-equal? (zip '(334 23 1 938) '(382 183 02 83)) '((334 382) (23 183) (1 2) (938 83)))
(check-equal? (zip '() '()) '())
