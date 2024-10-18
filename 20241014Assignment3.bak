#lang typed/racket

(require typed/rackunit)

; Data Definition
(define-type ExprC (U NumC binop IdC AppC ifleq0?))
(struct NumC ([n : Real]) #:transparent) ; EXPR = num
(struct binop ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ; Binary Operators
(struct IdC ([sym : Symbol]) #:transparent)
(struct AppC ([fun : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct ifleq0? ([x : ExprC][then : ExprC][else : ExprC]) #:transparent)
(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent) ; Functions

(define binop-table (hash '+ + '* * '/ /))

; Given an Sexp, return the ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(list (? symbol? sym) l r) (binop sym (parse (second s)) (parse (third s)))]
    [(? symbol? sym) (match sym
                          #; [(or binop-table ifleq0?)
                           (error "AAQZ : Invalid S-Expression")] 
                          [other (IdC s)])]  
    [other (error 'parse "AAQZ : Invalid S-Expression ~e" other)]))

; Given an Sexp, return the FundefC
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'def (? symbol? name)
           (list (list (? symbol? args) ...) '=> body))
     (cond [(check-duplicates args) (error 'parse-fundef "AAQZ : Duplicate Arguments in Function ~e" name)]
           [else (FunDefC name (cast args (Listof Symbol)) (parse body))])]
    [other (error 'parse-fundef "AAQZ : Invalid Function")]))

; Goes through a list of functions to return a list of FunDefC
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
     (match s
       ['() '()]
       [(? list? lst) (cons (parse-fundef (first lst)) (parse-prog (rest lst)))]
       ))

; Interprets the function named main from the fundefs
#; (define (interp-fns [funs : (Listof FunDefC)]) : Real
  )

#; (define (interp [exp : ExprC][funs : (Listof FunDefC)]) : Real
     (match exp
       [(NumC n) n]
       ;<idC-interp-case>
       ;<appC-interp-case>    
       [(binop '+ l r) (+ (interp l funs) (interp r funs))]
       [(binop '- l r) (- (interp l funs) (interp r funs))]
       [(binop '* l r) (* (interp l funs) (interp r funs))]
       [(binop '/ l r) (/ (interp l funs) (interp r funs))]))

; Replaces a parameter for ExprC into another ExprC
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) what]
    [(IdC n) (cond [(equal? n for) what]
                   [else in])]
    [(binop op l r) (binop op (subst what for l) (subst what for r))]))

; Get the FunDefC from a list given it's name
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds)
     (error 'get-fundef "AAQZ ： reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? n (FunDefC-name (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))


; Combines parsing and evaluation
#; (: top-interp (Sexp -> Real))
#;(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

; ----------------------------------------------------------------------------------
; ----------------------------------- TEST CASES -----------------------------------
; ----------------------------------------------------------------------------------


; parse
(check-equal? (parse '1) (NumC 1))
(check-equal? (parse '(+ 1 2)) (binop '+ (NumC 1) (NumC 2)))
(check-equal? (parse '(* 1 2)) (binop '* (NumC 1) (NumC 2)))
(check-equal? (parse '(+ (* 1 2) (+ 2 3)))
              (binop '+ (binop '* (NumC 1) (NumC 2))
                     (binop '+ (NumC 2) (NumC 3))))

(check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression '(1 2)"))
           (lambda () (parse '(1 2))))
#;(check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression 'test"))
           (lambda () (parse 'test)))
#;(check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression '+"))
           (lambda () (parse '+)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression '(+ 1)"))
           (lambda () (parse '(+ 1))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression '(1 + 2)"))
           (lambda () (parse '(1 + 2))))

; parse-fundef
(check-equal? (parse-fundef '{def f {() => 1}}) (FunDefC 'f '() (NumC 1)))
(check-equal? (parse-fundef '{def f {(x) => 1}}) (FunDefC 'f '(x) (NumC 1)))
(check-equal? (parse-fundef '{def f {(x y) => 1}}) (FunDefC 'f '(x y) (NumC 1)))
(check-equal? (parse-fundef '{def f {(x) => {+ 1 2}}}) (FunDefC 'f '(x) (binop '+ (NumC 1) (NumC 2))))
(check-equal? (parse-fundef '{def f {(x y) => {+ 1 2}}}) (FunDefC 'f '(x y) (binop '+ (NumC 1) (NumC 2))))
(check-equal? (parse-fundef '{def f {(x) => x}}) (FunDefC 'f '(x) (IdC 'x)))
(check-equal? (parse-fundef '{def f {(x) => {+ x 1}}}) (FunDefC 'f '(x) (binop '+ (IdC 'x) (NumC 1))))
(check-equal? (parse-fundef '{def f {(x y) => x}}) (FunDefC 'f '(x y) (IdC 'x)))
(check-equal? (parse-fundef '{def f {(x y) => {+ x y}}}) (FunDefC 'f '(x y) (binop '+ (IdC 'x) (IdC 'y))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function"))
           (lambda () (parse-fundef '{def f {x => 1}})))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function 'f"))
           (lambda () (parse-fundef '{def f {(x x) => 1}})))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function 'g"))
           (lambda () (parse-fundef '{def g {(x y x) => 1}})))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function 'g"))
           (lambda () (parse-fundef '{def g {(x y x) => x}})))



; parse-prog
(check-equal? (parse-prog '{{def f1 {() => 1}} {def f2 {() => 2}}})
              (list (FunDefC 'f1 '() (NumC 1)) (FunDefC 'f2 '() (NumC 2))))
#; (check-equal? (parse-prog '{{def f {(x) => 1}}}
                             {def main {() => {f 2}}})
                 (list (FunDefC 'f '(x) (NumC 1))
                       (FunDefC 'main () => 1)))


; subst
(check-equal? (subst (NumC 3) 'x (binop '+ (IdC 'x) (IdC 'x))) (binop '+ (NumC 3) (NumC 3)))
(check-equal? (subst (NumC 4) 'y (IdC 'y)) (NumC 4))
(check-equal? (subst (NumC 3) 'x (IdC 'y)) (IdC 'y))
(check-equal? (subst (NumC 1) 'x (binop '+ (IdC 'x) (IdC 'y))) (binop '+ (NumC 1) (IdC 'y)))


; top-interp
#; (check-equal? (top-interp '{def f {() => 1}}) 1)
#; (check-equal? (top-interp '{def f {(x) => 1}}) 1)
#; (check-equal? (top-interp '{def f {(x y) => 1}}) 1)
#; (check-equal? (top-interp '{def f {(x) => {+ 1 2}}}) 3)
#; (check-equal? (top-interp '{def f {(x y) => {+ 1 2}}}) 3)

#;(check-equal? (top-interp '{{def f {(x y) => {+ x y}}}
                     {def main {() => {f 1 2}}}}) 3)
#;(check-equal? (top-interp '{{def f {() => 5}}
                      {def main {() => {+ {f} {f}}}}}) 10)
#; (check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression '(1 + 2)"))
           (lambda () (top-interp '(1 + 2))))
#; (check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression '(1 2)"))
           (lambda () (top-interp '(1 2))))



