#lang typed/racket

(require typed/rackunit)
	
(define-type ExprC (U NumC IdC StrC IfC AppC LamC))
(struct NumC ([num : Real]) #:transparent) ; EXPR = num
(struct IdC ([sym : Symbol]) #:transparent)
(struct StrC ([str : String]) #:transparent)
(struct IfC ([x : ExprC] [then : ExprC] [else : ExprC]) #:transparent) ; do x if then is true, else else
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ((param : (Listof Symbol)) [body : ExprC]) #:transparent) ; Functions

; picks a random symbol from a list of symbols
(define (random-symbol): Symbol
  (list-ref (list 'a 'b 'c 'd 'e 'f 'g 'h) (random 8)))

; generates a random ExprC that contains no ExprC
(define (random-base-term) : ExprC
  (list-ref (list (NumC (random 10))
                  (IdC (random-symbol))
                  (StrC "random-base-term"))
            (random 3)))

; Creates an expression tree up to given depth
(define (random-term [depth : Real]) : ExprC
  (cond
    [(equal? depth 0) (random-base-term)]
    [else (list-ref (list (IfC (random-term (- depth 1)) (random-term (- depth 1)) (random-term (- depth 1)))
                  (AppC (random-term (- depth 1)) (list (random-term (- depth 1)) (random-term (- depth 1)) (random-term (- depth 1))))
                  (LamC (list (random-symbol) (random-symbol) (random-symbol)) (random-term (- depth 1))))
            (random 3))]))

; Takes AST to CST
(define (unparse [parsed : ExprC]) : Sexp
  (match parsed
    ; NumC
    [(NumC n) n]

    ; StrC
    [(StrC str) str]

    ; IfC
    [(IfC x then else) (list 'if (unparse x) (unparse then) (unparse else))]

    ; LamC
    [(LamC (list (? symbol? lst) ...) body) (list (cast lst (Listof Symbol)) '=> (unparse body))]

    ; AppC
    [(AppC fun (list param ...)) (cons (unparse fun) (map unparse param))]

    ; IdC
    [(IdC sym) sym]))


; Generates a random term, prints CST, and returns the term
(define (quiz) : ExprC
  (define exp (random-term (random 3)))
  (print (unparse exp))
  exp)

(define secret (quiz))

; ----------------------------------------------------------------------------------
; ----------------------------------- TEST CASES -----------------------------------
; ----------------------------------------------------------------------------------

#|
(random-symbol)
(random-base-term)
(random-term 5)
|#

; unparse
(check-equal? (unparse (NumC 1)) '1)
(check-equal? (unparse (IdC 'x)) 'x)
(check-equal? (unparse (StrC "string")) "string")
(check-equal? (unparse (StrC "string with spaces")) "string with spaces")
(check-equal? (unparse (IfC (NumC 1) (NumC 3) (NumC 0))) '(if 1 3 0))
(check-equal? (unparse (IfC (AppC (IdC '<=) (list (NumC 1) (NumC 2))) (NumC 3) (NumC 0))) '(if (<= 1 2) 3 0))
(check-equal? (unparse (AppC (IdC 'f) (list (NumC 2)))) '(f 2))
(check-equal? (unparse (AppC (IdC '+) (list (NumC 1) (NumC 2)))) '(+ 1 2))
(check-equal? (unparse (AppC (IdC '+) (list (NumC 1)))) '(+ 1)) ; valid in racket and AAQZ4
(check-equal? (unparse (LamC (list 'x) (NumC 1))) '({x} => 1))
(check-equal? (unparse (LamC (list 'x) (IdC 'x))) '({x} => x))
(check-equal? (unparse (LamC (list 'x 'y) (IdC 'x))) '({x y} => x))
(check-equal? (unparse (LamC (list 'x) (IdC '+))) '{{x} => +})
(check-equal? (unparse (LamC '() (NumC 0))) '{{} => 0})
(check-equal? (unparse (LamC (list '+) (NumC 0))) '{{+} => 0}) ; valid in racket and AAQZ4


