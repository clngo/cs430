#lang typed/racket

(require typed/rackunit)

; Progress Statement: Completely finished! All test cases pass! Thanks for the help on OH :D

; ----------------------------------- DATA DEFINITIONS -----------------------------------

(define-type ExprC (U NumC binop IdC AppC ifleq0?))
(struct NumC ([n : Real]) #:transparent) ; EXPR = num
(struct binop ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ; Binary Operators
(struct IdC ([sym : Symbol]) #:transparent)
(struct AppC ([fun : IdC] [args : (Listof ExprC)]) #:transparent)
(struct ifleq0? ([x : ExprC] [then : ExprC] [else : ExprC]) #:transparent) ; do x if then is true, else else

(struct FunDefC ([name : IdC] [args : (Listof IdC)] [body : ExprC]) #:transparent) ; Functions

; ----------------------------------- PARSING FUNCTIONS -----------------------------------

; Parses an S-expression and returns an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(list '+ l r) (binop '+ (parse l) (parse r))]
    [(list '- l r) (binop '- (parse l) (parse r))]
    [(list '* l r) (binop '* (parse l) (parse r))]
    [(list '/ l r) (binop '/ (parse l) (parse r))]
    [(list 'ifleq0? x then else) (ifleq0? (parse x) (parse then) (parse else))]
    [(? symbol? sym) (valid-id? sym) (IdC sym)]
    [(list (? symbol? id) args ...) (valid-id? id) (AppC (IdC id) (map parse args))]
    [other (error 'parse "AAQZ : Invalid S-Expression ~e" other)]))

; Checks if the given symbol is a valid identifier
(define (valid-id? [sym : Any]) : Boolean
  (match sym
    [(or 'def '=> '+ '- '* '/ 'ifleq0?) (error 'valid-id? "AAQZ : Invalid Id ~e" sym)]
    [else #t]))

; Checks if the list of symbols are valid-ids, and returns a list of IdC
(define (valid-args [syms : (Listof Symbol)]) : (Listof IdC)
  (match syms
    [(list) '()]
    [(cons (? valid-id? id) r) (cons (IdC id) (valid-args r))]))

; Parses a function definition S-expression into a FunDefC
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'def (? valid-id? name)
           (list (list (? symbol? args) ...) '=> body))
     (cond [(check-duplicates args) (error 'parse-fundef "AAQZ : Duplicate Arguments in Function ~e" name)]
           [else (FunDefC (IdC (cast name Symbol))
                          (valid-args (cast args (Listof Symbol)))
                          (parse body))])]
    [other (error 'parse-fundef "AAQZ : Invalid Function" )]))


; Parses multiple function definitions into a list of FunDefC
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    ['() '()]
    [(? list? lst) (cons (parse-fundef (first lst)) (parse-prog (rest lst)))]))

; ----------------------------------- INTERP FUNCTIONS -----------------------------------

; Interprets the function named main from the fundefs
(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (interp (FunDefC-body (get-fundef (IdC 'main) funs)) funs))
     
; Interprets an ExprC given a list of FunDefC to evaluate into a Real
(define (interp [exp : ExprC] [funs : (Listof FunDefC)]) : Real
  (match exp
    [(NumC n) n] 
    [(binop '+ l r) (+ (interp l funs) (interp r funs))]
    [(binop '- l r) (- (interp l funs) (interp r funs))]
    [(binop '* l r) (* (interp l funs) (interp r funs))]
    [(binop '/ l r) (define denom (interp r funs))
                    (cond [(zero? denom) (error 'interp "AAQZ : Cannot divide by 0")]
                          [else (/ (interp l funs) denom)])]
    [(ifleq0? x then else) (cond [(<= (interp x funs) 0) (interp then funs)]
                                 [else (interp else funs)])]
    [(AppC fun args) (define foundfun (get-fundef fun funs))
                     (cond [(equal? (length (FunDefC-args foundfun)) (length args))
                            (interp (subst-fn args
                                              (FunDefC-args foundfun)
                                              (FunDefC-body foundfun) funs) funs)]
                           [else (error 'interp "AAQZ : Wrong number of arguments")])]))

; Replaces a parameter for ExprC into another ExprC
(define (subst [what : ExprC] [for : ExprC] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(equal? in for) what]
               [else in])]
    [(binop op l r) (binop op (subst what for l) (subst what for r))]
    [(ifleq0? x then else) (ifleq0? (subst what for x) (subst what for then) (subst what for else))]
    [(AppC fun args) (AppC fun ((inst map ExprC ExprC)
                                (lambda (param) (subst what for param))
                                args))]))

; Substitution for mutliple parameters in function body
(define (subst-fn [args : (Listof ExprC)] [params : (Listof ExprC)] [body : ExprC] [funs : (Listof FunDefC)]) : ExprC
  (match params
    ['() body]
    [(cons f r) (subst-fn (rest args)
                          (rest params)
                          (subst (NumC (interp (first args) funs))
                                 (first params)
                                 body)
                          funs)]))

; Get the FunDefC from a list given it's function name
(define (get-fundef [n : IdC] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds)
     (error 'get-fundef "AAQZ : reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? n (FunDefC-name (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))

; Combines parsing and evaluation
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

; ----------------------------------------------------------------------------------
; ----------------------------------- TEST CASES -----------------------------------
; ----------------------------------------------------------------------------------

; List of functions for test cases 
(define fun-list
  (list (FunDefC (IdC 'f) '() (NumC 1))
        (FunDefC (IdC 'g) '() (binop '+ (NumC 1) (NumC 2)))
        (FunDefC (IdC 'h) (list (IdC 'x)) (NumC 1))
        (FunDefC (IdC 'i) (list (IdC 'x)) (IdC 'x))
        (FunDefC (IdC 'add) (list (IdC 'x) (IdC 'y)) (binop '+ (IdC 'x) (IdC 'y)))
        (FunDefC (IdC 'sub) (list (IdC 'x) (IdC 'y)) (binop '- (IdC 'x) (IdC 'y)))
        (FunDefC (IdC 'test1) (list (IdC 'x) (IdC 'y)) (ifleq0? (IdC 'x) (NumC 0) (AppC (IdC 'i) (list (IdC 'y)))))
        (FunDefC (IdC 'test2) (list (IdC 'x) (IdC 'y)) (binop '* (AppC (IdC 'add) (list (IdC 'x) (NumC 1)))
                                                              (AppC (IdC 'sub) (list (IdC 'y) (NumC 1)))))
        (FunDefC (IdC 'iftest) (list (IdC 'do) (IdC 'if) (IdC 'else)) (ifleq0? (IdC 'do) (IdC 'if) (IdC 'else)))
        (FunDefC (IdC 'main) '() (AppC (IdC 'f) '()))))

; ----------------------------------- TEST PARSING FUNCTIONS -----------------------------------

; parse
(check-equal? (parse '1) (NumC 1))
(check-equal? (parse '(+ 1 2)) (binop '+ (NumC 1) (NumC 2)))
(check-equal? (parse '(* 1 2)) (binop '* (NumC 1) (NumC 2)))
(check-equal? (parse '(+ (* 1 2) (+ 2 3)))
              (binop '+ (binop '* (NumC 1) (NumC 2))
                     (binop '+ (NumC 2) (NumC 3))))
(check-equal? (parse '(- 3 2)) (binop '- (NumC 3) (NumC 2)))
(check-equal? (parse '(/ 5 1)) (binop '/ (NumC 5) (NumC 1)))
(check-equal? (parse '(ifleq0? (+ 1 2) 3 0)) (ifleq0? (binop '+ (NumC 1) (NumC 2)) (NumC 3) (NumC 0)))
(check-equal? (parse 'x) (IdC 'x))
(check-equal? (parse 'y) (IdC 'y))
(check-equal? (parse '(f 2)) (AppC (IdC 'f) (list (NumC 2))))
(check-equal? (parse '(g 3 4)) (AppC (IdC 'g) (list (NumC 3) (NumC 4))))

(check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression '(1 2)"))
           (lambda () (parse '(1 2))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression '(1 + 2)"))
           (lambda () (parse '(1 + 2))))

; valid-id?
(check-equal? (valid-id? 'test) #t)
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id 'def"))
           (lambda () (valid-id? 'def)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id '+"))
           (lambda () (valid-id? '+)))

; valid-args
(check-equal? (valid-args '(a b c d e)) (list (IdC 'a) (IdC 'b) (IdC 'c) (IdC 'd)(IdC 'e)))

; parse-fundef
(check-equal? (parse-fundef '{def f {() => 1}})
              (FunDefC (IdC 'f) '() (NumC 1)))
(check-equal? (parse-fundef '{def f {(x) => 1}})
              (FunDefC (IdC 'f) (list (IdC 'x)) (NumC 1)))
(check-equal? (parse-fundef '{def f {(x y) => 1}})
              (FunDefC (IdC 'f) (list (IdC 'x) (IdC 'y)) (NumC 1)))
(check-equal? (parse-fundef '{def f {(x) => {+ 1 2}}})
              (FunDefC (IdC 'f) (list (IdC 'x)) (binop '+ (NumC 1) (NumC 2))))
(check-equal? (parse-fundef '{def f {(x y) => {+ 1 2}}})
              (FunDefC (IdC 'f) (list (IdC 'x) (IdC 'y)) (binop '+ (NumC 1) (NumC 2))))
(check-equal? (parse-fundef '{def f {(x) => {ifleq0? x 1 0}}})
              (FunDefC (IdC 'f) (list (IdC 'x)) (ifleq0? (IdC 'x) (NumC 1) (NumC 0))))
(check-equal? (parse-fundef '{def f {(x) => x}})
              (FunDefC (IdC 'f) (list (IdC 'x)) (IdC 'x)))
(check-equal? (parse-fundef '{def f {(x) => {+ x 1}}})
              (FunDefC (IdC 'f) (list (IdC 'x)) (binop '+ (IdC 'x) (NumC 1))))
(check-equal? (parse-fundef '{def f {(x y) => x}})
              (FunDefC (IdC 'f) (list (IdC 'x) (IdC 'y)) (IdC 'x)))
(check-equal? (parse-fundef '{def f {(x y) => {+ x y}}})
              (FunDefC (IdC 'f) (list (IdC 'x) (IdC 'y)) (binop '+ (IdC 'x) (IdC 'y))))
(check-equal? (parse-fundef '{def h {(a b c d e f g) => {ifleq0? 3 1 0}}})
              (FunDefC (IdC 'h) (list (IdC 'a) (IdC 'b) (IdC 'c) (IdC 'd) (IdC 'e) (IdC 'f) (IdC 'g))
                       (ifleq0? (NumC 3) (NumC 1) (NumC 0))))
(check-equal? (parse-fundef '{def f {(x y) => {- x y}}})
              (FunDefC (IdC 'f) (list (IdC 'x) (IdC 'y)) (binop '- (IdC 'x) (IdC 'y))))
(check-equal? (parse-fundef '{def f {(x y) => {* x y}}})
              (FunDefC (IdC 'f) (list (IdC 'x) (IdC 'y)) (binop '* (IdC 'x) (IdC 'y))))
(check-equal? (parse-fundef '{def f {(x y) => {/ x y}}})
              (FunDefC (IdC 'f) (list (IdC 'x) (IdC 'y)) (binop '/ (IdC 'x) (IdC 'y))))
(check-equal? (parse-fundef '{def f {(a x y) => {ifleq0? a x y}}})
              (FunDefC (IdC 'f) (list (IdC 'a)(IdC 'x) (IdC 'y)) (ifleq0? (IdC 'a) (IdC 'x) (IdC 'y))))

(check-exn (regexp (regexp-quote "AAQZ : Invalid Function"))
           (lambda () (parse-fundef '{def f {x => 1}})))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function"))
           (lambda () (parse-fundef '{def f {x 1}})))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function"))
           (lambda () (parse-fundef '{def f {x => {y}}})))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function"))
           (lambda () (parse-fundef '{def {x => 1}})))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function"))
           (lambda () (parse-fundef '{def f {x => "string"}})))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function 'f"))
           (lambda () (parse-fundef '{def f {(x x) => 1}})))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function 'g"))
           (lambda () (parse-fundef '{def g {(x y x) => 1}})))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function 'g"))
           (lambda () (parse-fundef '{def g {(x y x) => x}})))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id"))
           (lambda () (parse-fundef '{def f {(+) => x}})))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id"))
           (lambda () (parse-fundef '{def def {(x) => x}})))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id"))
           (lambda () (parse-fundef '{def + {(x) => x}})))
#; (check-exn (regexp (regexp-quote "AAQZ : Invalid Id"))
              (lambda () (parse-fundef '{def 1 {(x) => x}})))
; parse-prog
(check-equal? (parse-prog '{{def f1 {() => 1}} {def f2 {() => 2}}})
              (list (FunDefC (IdC 'f1) '() (NumC 1)) (FunDefC (IdC 'f2) '() (NumC 2))))
(check-equal? (parse-prog '{{def f {(x) => 1}}
                            {def main {() => {f 2}}}})
              (list (FunDefC (IdC 'f) (list (IdC 'x)) (NumC 1))
                    (FunDefC (IdC 'main) '() (AppC (IdC 'f) (list (NumC 2))))))

; ----------------------------------- TEST INTERP FUNCTIONS -----------------------------------

; interp
(check-equal? (interp (binop '+ (NumC 5) (NumC 0)) fun-list) 5)
(check-equal? (interp (binop '+ (NumC 5) (NumC 10)) fun-list) 15)
(check-equal? (interp (binop '+ (binop '+ (NumC 5) (NumC 0)) (NumC 3)) fun-list) 8)
(check-equal? (interp (binop '+ (NumC 3) (binop '+ (NumC 5) (NumC 0))) fun-list) 8)
(check-equal? (interp (binop '* (NumC 5) (NumC 3)) fun-list) 15)
(check-equal? (interp (binop '* (NumC 0) (NumC 9103413840310)) fun-list) 0)
(check-equal? (interp (binop '* (binop '+ (NumC 2) (NumC 5)) (binop '* (NumC 3) (NumC 1))) fun-list) 21)
(check-equal? (interp (AppC (IdC 'f) '()) fun-list) 1)
(check-equal? (interp (AppC (IdC 'g) '()) fun-list) 3)
(check-equal? (interp (AppC (IdC 'h) (list (NumC 293048))) fun-list) 1)
(check-equal? (interp (AppC (IdC 'i) (list (NumC 293048))) fun-list) 293048)
(check-equal? (interp (AppC (IdC 'add) (list (NumC 13) (NumC 23))) fun-list) 36)
(check-equal? (interp (AppC (IdC 'sub) (list (NumC 23) (NumC 13))) fun-list) 10)
(check-equal? (interp (AppC (IdC 'iftest)
                            (list (NumC 0) (AppC (IdC 'add) (list (NumC 13) (NumC 23))) (NumC 0))) fun-list) 36)
(check-equal? (interp (AppC (IdC 'iftest)
                            (list (NumC 9) (AppC (IdC 'add) (list (NumC 13) (NumC 23))) (NumC 0))) fun-list) 0)
(check-equal? (interp (AppC (IdC 'test1) (list (NumC 1) (NumC 2))) fun-list) 2)
(check-equal? (interp (AppC (IdC 'test2) (list (NumC 3) (NumC 4))) fun-list) 12)
(check-equal? (interp (AppC (IdC 'main) '()) fun-list) 1)

(check-exn (regexp (regexp-quote "AAQZ : Wrong number of arguments"))
           (lambda () (interp (AppC (IdC 'f) (list (NumC 1))) fun-list)))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of arguments"))
           (lambda () (interp (AppC (IdC 'g) (list (NumC 1))) fun-list)))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of arguments"))
           (lambda () (interp (AppC (IdC 'h) '()) fun-list)))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of arguments"))
           (lambda () (interp (AppC (IdC 'i) (list (NumC 1) (NumC 2))) fun-list)))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of arguments"))
           (lambda () (interp (AppC (IdC 'add) (list (NumC 1) (NumC 2) (NumC 3))) fun-list)))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of arguments"))
           (lambda () (interp (AppC (IdC 'sub) (list (NumC 1) (NumC 2) (NumC 3))) fun-list)))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of arguments"))
           (lambda () (interp (AppC (IdC 'f) (list (NumC 1))) fun-list)))
(check-exn (regexp (regexp-quote "AAQZ : Cannot divide by 0"))
           (lambda () (interp (binop '/ (NumC 1) (NumC 0)) fun-list)))
(check-exn (regexp (regexp-quote "AAQZ : Cannot divide by 0"))
           (lambda () (interp (binop '/ (NumC 1) (binop '+ (NumC 0) (NumC 0))) fun-list)))
; subst
(check-equal? (subst (NumC 3) (IdC 'x) (binop '+ (IdC 'x) (IdC 'x))) (binop '+ (NumC 3) (NumC 3)))
(check-equal? (subst (NumC 4) (IdC 'y) (IdC 'y)) (NumC 4))
(check-equal? (subst (NumC 3) (IdC 'x) (IdC 'y)) (IdC 'y))
(check-equal? (subst (NumC 1) (IdC 'x) (binop '+ (IdC 'x) (IdC 'y))) (binop '+ (NumC 1) (IdC 'y)))
(check-equal? (subst (NumC 1) (IdC 'x) (binop '- (IdC 'x) (IdC 'y))) (binop '- (NumC 1) (IdC 'y)))
(check-equal? (subst (NumC 1) (IdC 'x) (binop '* (IdC 'x) (IdC 'y))) (binop '* (NumC 1) (IdC 'y)))
(check-equal? (subst (NumC 1) (IdC 'x) (binop '/ (IdC 'x) (IdC 'y))) (binop '/ (NumC 1) (IdC 'y)))
(check-equal? (subst (NumC 1) (IdC 'x) (ifleq0? (IdC 'x) (IdC 'y) (NumC 2))) (ifleq0? (NumC 1) (IdC 'y) (NumC 2)))

; subst-fn
(check-equal? (subst-fn (list (NumC 1)) (list (IdC 'x)) (IdC 'x) fun-list)
              (NumC 1))
(check-equal? (subst-fn (list (NumC '1) (NumC '2)) (list (IdC 'x) (IdC 'y)) (binop '+ (IdC 'x) (IdC 'y)) fun-list)
              (binop '+ (NumC 1) (NumC 2)))
(check-equal? (subst-fn (list (NumC 0) (NumC 999))
                        (list (IdC 'x) (IdC 'y))
                        (ifleq0? (IdC 'x) (IdC 'y) (NumC 2))
                        fun-list)
              (ifleq0? (NumC 0) (NumC 999) (NumC 2)))
(check-equal? (subst-fn (list (binop '+ (NumC 1) (NumC 3)) (binop '* (NumC 5) (NumC 2)))
                        (list (IdC 'x) (IdC 'y))
                        (binop '- (IdC 'y) (IdC 'x)) fun-list)
              (binop '- (NumC 10) (NumC 4)))

; get-fundef
(check-equal? (get-fundef (IdC 'f) fun-list) (FunDefC (IdC 'f) '() (NumC 1)))
(check-equal? (get-fundef (IdC 'g) fun-list) (FunDefC (IdC 'g) '() (binop '+ (NumC 1) (NumC 2))))
(check-equal? (get-fundef (IdC 'h) fun-list) (FunDefC (IdC 'h) (list (IdC 'x)) (NumC 1)))
(check-equal? (get-fundef (IdC 'i) fun-list) (FunDefC (IdC 'i) (list (IdC 'x)) (IdC 'x)))
(check-exn (regexp (regexp-quote "AAQZ : reference to undefined function"))
           (lambda () (get-fundef (IdC 'abc) fun-list)))

; interp-fns
(check-equal? (interp-fns fun-list) 1)
(check-equal? (interp-fns (list (FunDefC (IdC 'main) '() (binop '+ (NumC 1) (NumC 2))))) 3)

; top-interp
(check-equal? (top-interp '{{def f {(x y) => {+ x y}}}
                            {def main {() => {f 1 2}}}}) 3)
(check-equal? (top-interp '{{def f {() => 5}}
                            {def main {() => {+ {f} {f}}}}}) 10)
(check-equal? (top-interp '{{def ifthen {(x) => {ifleq0? x x 99999999}}}
                            {def main {() => {ifthen 4}}}}) 99999999) 
(check-equal? (top-interp '{{def ifthen {(x) => {ifleq0? x x 99999999}}}
                            {def main {() => {ifthen -55}}}}) -55) 
(check-equal? (top-interp '{{def g {() => {+ 1 2}}}
                            {def main {() => {g}}}}) 3)
(check-equal? (top-interp '{{def h {(x) => 1}}
                            {def main {() => {h 0934}}}}) 1)
(check-equal? (top-interp '{{def i {(x) => x}}
                            {def main {() => {i -294038}}}}) -294038)
(check-equal? (top-interp '{{def sub {(x y) => {- x y}}}
                            {def main {() => {sub 243897 4843}}}}) 239054)
(check-equal? (top-interp '{{def mult {(x y) => {* x y}}}
                            {def main {() => {mult 2 8}}}}) 16)
(check-equal? (top-interp '{{def div {(x y) => {/ x y}}}
                            {def main {() => {div 16 2}}}}) 8)
(check-equal? (top-interp '{{def add {(x y) => {+ x y}}}
                            {def sub {(x y) => {- x y}}}
                            {def mult {(x y) => {* x y}}}
                            {def div {(x y) => {/ x y}}}
                            {def f1 {(x y) => {add {sub {add x y} {sub x y}} {div {mult x y} 2}}}}
                            {def main {() => {f1 2 2}}}}) 6)
(check-equal? (top-interp '{{def add {(x y) => {+ x y}}}
                            {def sub {(x y) => {- x y}}}
                            {def mult {(x y) => {* x y}}}
                            {def div {(x y) => {/ x y}}}
                            {def f1 {(x y) => {add {sub {add x y} {sub x y}} {div {mult x y} 2}}}}
                            {def main {() => {f1 4 4}}}}) 16)
(check-equal? (top-interp '{{def add {(x y) => {+ x y}}}
                            {def sub {(x y) => {- x y}}}
                            {def mult {(x y) => {* x y}}}
                            {def div {(x y) => {/ x y}}}
                            {def f1 {(x y) => {add {sub {add x y} {sub x y}} {div {mult x y} 2}}}}
                            {def main {() => {f1 6 2}}}}) 10)
(check-equal? (top-interp '{{def add {(x y) => {+ x y}}}
                            {def sub {(x y) => {- x y}}}
                            {def mult {(x y) => {* x y}}}
                            {def div {(x y) => {/ x y}}}
                            {def f1 {(x y) => {add {sub {add x y} {sub x y}} {div {mult x y} 2}}}}
                            {def main {() => {f1 6 2}}}}) 10)
(check-equal? (top-interp '{{def percent {(num den) => {* {/ num den} 100}}}
                            {def main {() => {percent 36 60}}}}) 60)
(check-equal? (top-interp '{{def addfive {(x) => {+ x 5}}}
                            {def main {() => {addfive 3}}}}) 8)
(check-exn (regexp (regexp-quote "AAQZ : Cannot divide by 0"))
           (lambda ()(top-interp '{{def ignoreit ((x) => (+ 3 4))}
                                   {def main {() => (ignoreit (/ 1 (+ 0 0)))}}})))