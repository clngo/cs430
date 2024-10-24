#lang typed/racket

(require typed/rackunit)
	
(define-type ExprC (U NumC IdC StrC IfC AppC LamC))
(struct NumC ([num : Real]) #:transparent) ; EXPR = num
(struct IdC ([sym : Symbol]) #:transparent)
(struct StrC ([str : String]) #:transparent)
(struct IfC ([x : ExprC] [then : ExprC] [else : ExprC]) #:transparent) ; do x if then is true, else else
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ((param : (Listof Symbol)) [body : ExprC]) #:transparent) ; Functions

; Values
(define-type Value (U NumV PrimV CloV BoolV StrV))
(struct NumV ([num : Real]) #:transparent)
(struct PrimV ([sym : Symbol]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct CloV ([param : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct BoolV ([bool : Boolean]) #:transparent)

(struct Binding ([name : Symbol] [val : Value]) #:transparent)
(define-type Env (Listof Binding))
(define extend-env cons)
(define top-env
  (list (Binding '+ (PrimV '+))
        (Binding '- (PrimV '-))
        (Binding '* (PrimV '*))
        (Binding '/ (PrimV '/))
        (Binding '<= (PrimV '<=))
        (Binding 'equal? (PrimV 'equal?))
        (Binding 'true (BoolV #t))
        (Binding 'false (BoolV #f))
        (Binding 'error (PrimV 'error))))

; Parses an S-expression and returns an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    ; NumC
    [(? real? n) (NumC n)]

    ; StrC
    [(? string? str) (StrC str)]

    ; IfC
    [(list 'if x then else) (IfC (parse x) (parse then) (parse else))]

    ; Bind
    [(list 'bind clauses ... body) (StrC "testing")]
    
    ; LamC
    [(list (list (? symbol? param) ...) '=> body)
     (cond [(check-duplicates param) (error 'parse "AAQZ : Duplicate Arguments in Function")]
           [else (LamC (valid-args (cast param (Listof Symbol))) (parse body))])]

    ; AppC
    [(list fun param ...) (AppC (parse fun) (map parse param))]

    ; IdC
    [(? symbol? sym) (IdC sym)]
    [other (error 'parse "AAQZ : Invalid S-Expression ~e" other)]))


; Checks if the given symbol is a valid identifier
(define (valid-id? [sym : Any]) : Boolean
  (match sym
    [(or 'if '=> 'bind '=) (error 'valid-id? "AAQZ : Invalid Id ~e" sym)]
    [else #t]))

; Checks if the list of symbols are valid-ids, and returns a list of IdC
(define (valid-args [syms : (Listof Symbol)]) : (Listof Symbol)
  (match syms
    [(list) '()]
    [(cons (? valid-id? id) r) (cons id (valid-args r))]))

; ----------------------------------- INTERP FUNCTIONS -----------------------------------

; Turns value into string
(define (serialize [val : Value]) : String
  (match val
    [(NumV n) (format "~v" n)]
    [(StrV str) (format "~v" str)]
    [(PrimV op) "#<primop>"]
    [(BoolV b) (if b "true" "false")]
    [(CloV param body env) "#<procedure>"]))


; Interprets an ExprC given a list of FunDefC to evaluate into a Real
(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    [(NumC num) (NumV num)]
    [(StrC str) (StrV str)]
    [(IdC sym) (lookup sym env)]
    [(IfC x t e) (define boo (interp x env))
                       (cond [BoolV? boo
                                     (cond [(equal? boo (BoolV #t)) (interp t env)]
                                           [else (interp e env)])]
                             [else (error 'interp "AAQZ : Given non-boolean type")])]
    [(LamC p b) (CloV p b env)]
    [(AppC f a) (define f-value (interp f env))
                (define val-lst (map (lambda ([arg : ExprC]) (interp arg env)) a))
                (match f-value
                  [(CloV p b e)
                   (cond
                     [(equal? (length p) (length val-lst)) (interp b (extend-helper p val-lst e))]
                     [else (error 'interp "AAQZ : Wrong number of args")])]
                  [(PrimV sym) (cond [(equal? (length val-lst) 2)
                                      (interp-PrimV sym (first val-lst) (first (rest val-lst)))]
                                     [(equal? (length val-lst) 1)
                                      (match sym
                                        ['error (error 'interp "AAQZ : user-error ~s" (serialize (first val-lst)))]
                                        [other (error 'interp "AAQZ : Wrong number of args")])]
                                     [else (error 'interp "AAQZ : Wrong number of args")])]
                  [other (error 'interp "AAQZ : AppC is wrong")])]))

; Finds binding in env based given a symbol
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "AAQZ : name not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val]
                                   [else (lookup for r)])]))


; adds new bindings to the environment
(define (extend-helper [args : (Listof Symbol)] [vals : (Listof Value)] [env : Env]) : Env
  ; assume both lists are equal length
  (match* (args vals)
    [((cons f-a r-a) (cons f-v r-v)) (extend-helper r-a r-v (extend-env (Binding f-a f-v) env))]
    [('() '()) env]))

; helper for PrimV
(define (interp-PrimV [sym : Symbol] [l : Value] [r : Value]) : Value
  (match sym
    ['+ (cond [(and (NumV? l) (NumV? r)) (NumV (+ (NumV-num l) (NumV-num r)))]
              [else (error 'interp-PrimV "one argument was not a number")])]
    ['- (cond [(and (NumV? l) (NumV? r)) (NumV (- (NumV-num l) (NumV-num r)))]
              [else (error 'interp-PrimV "one argument was not a number")])]
    ['* (cond [(and (NumV? l) (NumV? r)) (NumV (* (NumV-num l) (NumV-num r)))]
              [else (error 'interp-PrimV "one argument was not a number")])]
    ['/ (cond [(and (NumV? l) (NumV? r))
               (cond [(zero? (NumV-num r)) (error 'interp-PrimV "AAQZ : Cannot divide by 0")]
                     [else (NumV (/ (NumV-num l) (NumV-num r)))])]
              [else (error 'interp-PrimV "one argument was not a number")])]
    ['<= (cond [(and (NumV? l) (NumV? r)) (BoolV (<= (NumV-num l) (NumV-num r)))]
               [else (error 'interp-PrimV "one argument was not a number")])]
    ; Functions like eq?
    ; Neither l or r is a CloV or PrimV and 2 values are equal
    ['equal? (cond [(and (and (not (CloV? l)) (not (PrimV? l))) (and (not (CloV? r)) (not (PrimV? r))))
                    (BoolV (equal? l r))]
                   [else (BoolV #f)])]
    [other (error 'interp-PrimV "AAQZ : Wrong number of args")]))


; Combines parsing and evaluation
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

; ----------------------------------------------------------------------------------
; ----------------------------------- TEST CASES -----------------------------------
; ----------------------------------------------------------------------------------

; parse
(check-equal? (parse '1) (NumC 1))
(check-equal? (parse 'x) (IdC 'x))
(check-equal? (parse 'y) (IdC 'y))
(check-equal? (parse "string") (StrC "string"))
(check-equal? (parse "string with spaces") (StrC "string with spaces"))
(check-equal? (parse '(if 1 3 0)) (IfC (NumC 1) (NumC 3) (NumC 0)))
(check-equal? (parse '(if (<= 1 2) 3 0))
              (IfC (AppC (IdC '<=) (list (NumC 1) (NumC 2))) (NumC 3) (NumC 0)))
(check-equal? (parse '(f 2)) (AppC (IdC 'f) (list (NumC 2))))
(check-equal? (parse '(g 3 4)) (AppC (IdC 'g) (list (NumC 3) (NumC 4))))
(check-equal? (parse '(+ 1 2)) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '(<= 1 2)) (AppC (IdC '<=) (list (NumC 1) (NumC 2))))
(check-equal? (parse '(+ 1)) (AppC (IdC '+) (list (NumC 1)))) ; valid in racket, invalid AAQZ4???
(check-equal? (parse '({x} => 1)) (LamC (list 'x) (NumC 1)))
(check-equal? (parse '({x} => x)) (LamC (list 'x) (IdC 'x)))
(check-equal? (parse '{{x} => +}) (LamC (list 'x) (IdC '+)))
(check-equal? (parse '{{} => 0}) (LamC '() (NumC 0)))
#; (check-equal? (parse '{{a} => {+ a}}) (LamC (list (IdC 'a)) (NumC 5))) ; ?????????????????
(check-equal? (parse '{{+} => 0}) (LamC (list '+) (NumC 0))) ; valid in racket, invalid AAQZ4???

  
; valid-id?
(check-equal? (valid-id? 'test) #t)
(check-equal? (valid-id? '+) #t)
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id 'if"))
           (lambda () (valid-id? 'if)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id '=>"))
           (lambda () (valid-id? '=>)))

; valid-args
(check-equal? (valid-args '(a b c d e)) (list 'a 'b 'c 'd 'e))

; interp
(check-equal? (interp (NumC 1) top-env) (NumV 1))
(check-equal? (interp (StrC "test") top-env) (StrV "test"))
(check-equal? (interp (IdC '+) top-env) (PrimV '+))
(check-equal? (interp (IdC '-) top-env) (PrimV '-))
(check-equal? (interp (IdC 'error) top-env) (PrimV 'error))
(check-equal? (interp (IdC 'true) top-env) (BoolV #t))
(check-equal? (interp (IdC 'false) top-env) (BoolV #f))
(check-exn (regexp (regexp-quote "AAQZ : name not found: 'x"))
           (lambda () (interp (IdC 'x) top-env)))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (NumC 1) (NumC 2))) (NumC 3) (NumC 0)) top-env)
                      (NumV 3))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (NumC 2) (NumC 1))) (NumC 3) (NumC 0)) top-env)
                      (NumV 0))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (NumC 2) (NumC 1))) (IdC 'true) (IdC 'false)) top-env)
                      (BoolV #f))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (NumC 2) (NumC 2))) (IdC 'true) (IdC 'false)) top-env)
                      (BoolV #t))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 2)))
                           (IdC 'true) (StrC "False PrimV")) top-env)
                      (StrV "False PrimV"))
(check-equal? (interp (LamC (list 'x) (NumC 1)) top-env) (CloV (list 'x) (NumC 1) top-env))
(check-equal? (interp (LamC '() (StrC "Call with no args")) top-env)
              (CloV '() (StrC "Call with no args") top-env))
(check-equal? (interp (AppC (IdC '+) (list (NumC 1) (NumC 0))) top-env) (NumV 1))
(check-equal? (interp (AppC (IdC '-) (list (NumC 5) (NumC 3))) top-env) (NumV 2))
(check-equal? (interp (AppC (IdC '+) (list (NumC 1) (NumC 0))) top-env) (NumV 1))
(check-equal? (interp (AppC (LamC (list 'x) (IdC 'x)) (list (NumC 1))) top-env) (NumV 1))
(check-equal? (interp (AppC (LamC (list 'x) (IdC 'x)) (list (NumC 1))) top-env) (NumV 1))
(check-equal? (interp (AppC (LamC (list 'x 'y)
                                  (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
                            (list (NumC 1) (NumC 2))) top-env)
              (NumV 3))
(check-equal? (interp (AppC (LamC (list '+ '-)
                                  (AppC (IdC '*) (list (IdC '+) (IdC '-))))
                            (list (NumC 5) (NumC 3))) top-env)
              (NumV 15))
(check-equal? (interp (AppC (LamC (list 'x)
                                  (AppC (IdC '+) (list (IdC 'x) (NumC 1))))
                            (list (NumC 7))) top-env)
              (NumV 8)) 
#; (check-equal? (interp (AppC (LamC (list 'f 'g)
                                  (LamC (list 'x)
                                        (AppC (IdC 'f)
                                              (list (AppC (IdC 'g) (list (IdC 'x)))))))
                            (list (LamC (list 'f)
                                        (AppC (IdC '+) (list (IdC 'y) (NumC 1))))
                                  (LamC (list 'z) (AppC (IdC '+) (list (IdC 'z) (NumC 2)))))) top-env)
              (NumV 4))  ; f(g(x)) ; g(x) = x + 2; f(x) = x + 1
(check-exn (regexp (regexp-quote "AAQZ : user-error \"1\""))
           (lambda () (interp (AppC (IdC 'error) (list (NumC 1))) top-env)))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of args"))
           (lambda () (interp (AppC (IdC '+) (list (NumC 1))) top-env)))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of args"))
           (lambda () (interp (AppC (IdC 'error) (list (NumC 1) (NumC 2))) top-env)))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of args"))
           (lambda () (interp (AppC (IdC 'error) (list (NumC 1) (NumC 2) (NumC 3))) top-env)))



; interp-PrimV
(check-equal? (interp-PrimV '+ (NumV 1) (NumV 0)) (NumV 1))
(check-equal? (interp-PrimV '- (NumV 5) (NumV 3)) (NumV 2))
(check-equal? (interp-PrimV '* (NumV 5) (NumV 3)) (NumV 15))
(check-equal? (interp-PrimV '/ (NumV 15) (NumV 3)) (NumV 5))
(check-exn (regexp (regexp-quote "AAQZ : Cannot divide by 0"))
           (lambda () (interp-PrimV '/ (NumV 8329) (NumV 0))))
(check-equal? (interp-PrimV '<= (NumV 5) (NumV 3)) (BoolV #f))
(check-equal? (interp-PrimV '<= (NumV 3) (NumV 5)) (BoolV #t))
(check-equal? (interp-PrimV 'equal? (NumV 3) (NumV 5)) (BoolV #f))
(check-equal? (interp-PrimV 'equal? (NumV 5) (NumV 5)) (BoolV #t))
(check-equal? (interp-PrimV 'equal? (StrV "test1") (StrV "test2")) (BoolV #f))
(check-equal? (interp-PrimV 'equal? (StrV "test1") (StrV "test1")) (BoolV #t))
(check-equal? (interp-PrimV 'equal? (PrimV '+) (StrV "test1")) (BoolV #f))
(check-equal? (interp-PrimV 'equal? (CloV '() (NumC 1) top-env) (NumV 1)) (BoolV #f))



; lookup
(check-equal? (lookup 'test (list (Binding 'test (NumV 1)))) (NumV 1))
(check-equal? (lookup 'test (list (Binding 'test1 (NumV 1))
                                  (Binding 'test2 (NumV 2))
                                  (Binding 'test (NumV 3)))) (NumV 3))
(check-exn (regexp (regexp-quote "AAQZ : name not found: 'test"))
           (lambda () (lookup 'test (list (Binding 'failtest (NumV 1))))))

; serialize
(check-equal? (serialize (NumV 023948)) "23948")
(check-equal? (serialize (PrimV '+)) "#<primop>")
(check-equal? (serialize (StrV "testing")) "\"testing\"")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (CloV (list 'x) (IdC 'x) top-env)) "#<procedure>")
(check-equal? (serialize (CloV (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))) top-env)) "#<procedure>")

; top-interp
(check-equal? (top-interp 3) "3")
(check-equal? (top-interp "test") "\"test\"")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp '(if (<= 0 1) 0 1)) "0")
(check-equal? (top-interp '(if (<= 1 0) 0 1)) "1")
(check-equal? (top-interp '(+ 1 2)) "3")
(check-equal? (top-interp '(((a) => 1) 1)) "1")
(check-equal? (top-interp '(((a) => a) 1)) "1")
(check-equal? (top-interp '(((a b) => (+ a b)) 1 2)) "3")
(check-equal? (top-interp '(((a b) => (- a b)) 3 2)) "1")
(check-equal? (top-interp '(((a b) => (equal? a b)) 3 2)) "false")
(check-equal? (top-interp '(((a b) => (if (<= a b) "testing is true" "b is smaller")) 1 2)) "\"testing is true\"")
(check-equal? (top-interp '(((a b) => (if (<= a b) "testing is true" "b is smaller")) 2 1)) "\"b is smaller\"")
(check-equal? (top-interp '(((x y) => (if (equal? x y) "match" "no match")) 3 3)) "\"match\"")
(check-equal? (top-interp '(((x y) => (if (equal? x y) "match" "no match")) 3 4)) "\"no match\"")
(check-equal? (top-interp '(((x) => (* x x)) 4)) "16")
(check-equal? (top-interp '(((x) => (((y) => (+ x y)) 10)) 5)) "15")
#; (check-equal? (top-interp '(((n) => {if {<= n 0} 1 {* n {fact {- n 1}}}}) 4)) "error")
(check-equal? (top-interp '(((f g) => (((x) => (g (f x))) 2))
                            ((x) => (+ x 3))
                            ((y) => (* y 2))))
              "10")
