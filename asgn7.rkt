#lang typed/racket

; Completed and passed all test cases! :D

(require typed/rackunit)

(define-type ExprC (U NumC IdC StrC IfC AppC LamC RecC))
(struct NumC ([num : Real]) #:transparent) ; EXPR = num
(struct IdC ([sym : Symbol]) #:transparent)
(struct StrC ([str : String]) #:transparent)
(struct IfC ([x : ExprC] [then : ExprC] [else : ExprC]) #:transparent) ; do x if then is true, else else
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ([param : (Listof Symbol)] [types : (Listof Type)] [body : ExprC]) #:transparent) ; Functions
(struct RecC ([f : Symbol]
              [a : (Listof Symbol)]
              [aT : (Listof Type)]
              [rT : Type]
              [body : ExprC]
              [use : ExprC]) #:transparent) 

; Values
(define-type Value (U NumV PrimV CloV BoolV StrV))
(struct NumV ([num : Real]) #:transparent)
(struct PrimV ([sym : Symbol]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct CloV ([param : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct BoolV ([bool : Boolean]) #:transparent)

; Types
(define-type Type (U NumT BoolT StrT FunT))
(struct NumT () #:transparent)
(struct BoolT () #:transparent)
(struct StrT () #:transparent)
(struct FunT ([in : (Listof Type)] [out : Type]) #:transparent)

; Type-Environment
(struct Type-Binding ([name : Symbol] [type : Type]) #:transparent)
(define-type Tenv (Listof Type-Binding))
(define extend-tenv cons)
(define base-tenv
  (list (Type-Binding '+ (FunT (list (NumT) (NumT)) (NumT)))
        (Type-Binding '- (FunT (list (NumT) (NumT)) (NumT)))
        (Type-Binding '* (FunT (list (NumT) (NumT)) (NumT)))
        (Type-Binding '/ (FunT (list (NumT) (NumT)) (NumT)))
        (Type-Binding '<= (FunT (list (NumT) (NumT)) (BoolT)))
        (Type-Binding 'num-eq? (FunT (list (NumT) (NumT)) (BoolT)))
        (Type-Binding 'str-eq? (FunT (list (StrT) (StrT)) (BoolT)))
        (Type-Binding 'true (BoolT))
        (Type-Binding 'false (BoolT))))

; Environment
(struct Binding ([name : Symbol] [loc : Location]) #:transparent)
(define-type Env (Listof Binding))
(define extend-env cons)
(define top-env
  (list (Binding '+ 1)
        (Binding '- 2)
        (Binding '* 3)
        (Binding '/ 4)
        (Binding '<= 5)
        (Binding 'num-eq? 6)
        (Binding 'str-eq? 7)
        (Binding 'true 8)
        (Binding 'false 9)))


; Store
(define-type Location Natural)
(define-type Store (Mutable-Vectorof Value))
(define mt-store empty)
(define override-store cons)


; ----------------------------------- PARSE FUNCTIONS -----------------------------------


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
    [(list 'bind clauses ... body)
     (define symbols (extract-symbols (cast clauses Sexp) '()))
     (define types (extract-types (cast clauses Sexp) '()))
     (define exprs (extract-expressions (cast clauses Sexp) '()))
     (if (equal? (length symbols) (length (remove-duplicates symbols)))
         (AppC (LamC symbols types (parse body)) exprs)
         (error 'parse "AAQZ : Duplicate Arguments in Function ~a" symbols))]

    ; RecBind
    [(list 'recbind (list (? symbol? fn-name) ': type '= exp) body)
     (define parsed-exp (parse exp))
     (match parsed-exp
       [(LamC p t b)
        (RecC fn-name p t (parse-type type) parsed-exp (parse body))]
       [other (error 'parse "AAQZ : recbind invalid function definition")])]
    
    ; LamC
    [(list (list param ...) '=> body)
     (define idparam (check-dup-helper param)) ; (list 'x 'y)
     (cond [(check-duplicates idparam) (error 'parse "AAQZ : Duplicate Arguments in Function ~a" param)]
           [else (LamC (valid-args idparam)
                       (parse-param-types param)
                       (parse body))])]

    ; AppC
    [(list fun param ...) (AppC (parse fun) (map parse param))]

    ; IdC
    [(? symbol? sym) (valid-id? sym) (IdC sym)]
    
    [other (error 'parse "AAQZ : Invalid S-Expression ~e" other)]))

;; Helper for desugaring that extracts symbols for LamC's parameters
(define (extract-symbols [binding-clauses : Sexp] [accumulated-ids : (Listof Symbol)]) : (Listof Symbol)
  (match binding-clauses
    [(list (list (? symbol? ids) ': type '= expr))
     (valid-id? ids) (extract-symbols '() (cons ids accumulated-ids))]
    [(list (list (? symbol? ids) ': type '= expr) binding-clauses ...)
     (extract-symbols binding-clauses (cons ids accumulated-ids))]
    ['() (reverse accumulated-ids)]
    [other (error 'extract-symbols "AAQZ : Invalid binding clause structure: ~a" binding-clauses)]))

;; Helper for desugaring that extracts types for LamC's parameters
(define (extract-types [binding-clauses : Sexp] [accumulated-types : (Listof Type)]) : (Listof Type)
  (match binding-clauses
    ['() (reverse accumulated-types)]
    [(list (list (? symbol? ids) ': type '= expr))
     (extract-types '() (cons (parse-type type) accumulated-types))]
    [(list (list (? symbol? ids) ': type '= expr) binding-clauses ...)
     (extract-types binding-clauses (cons (parse-type type) accumulated-types))]
    [other (error 'extract-types "AAQZ : Invalid binding clause structure: ~a" binding-clauses)]))

;; Helper for desugaring that extracts expressions for LamC's parameters
(define (extract-expressions [clauses : Sexp] [accumulated-exprs : (Listof ExprC)]) : (Listof ExprC)
  (match clauses
    [(list (list (? symbol? ids) ': type '= exp))
     (extract-expressions '() (cons (parse exp) accumulated-exprs))]
    [(list (list (? symbol? ids) ': type '= exp) clauses ...)
     (extract-expressions clauses (cons (parse exp) accumulated-exprs))]
    ['() (reverse accumulated-exprs)]
    [other (error 'extract-expressions "AAQZ : Invalid binding clause structure: ~a" clauses)]))

; Checks if the given symbol is a valid identifier
(define (valid-id? [sym : Any]) : Boolean
  (match sym
    [(or 'if 'bind 'recbind '=> '= ':) (error 'valid-id? "AAQZ : Invalid Id ~e" sym)]
    [else #t]))

; Checks if the list of symbols are valid-ids, and returns a list of IdC
(define (valid-args [syms : (Listof Symbol)]) : (Listof Symbol)
  (match syms
    [(list) '()]
    [(cons (? valid-id? id) r) (cons id (valid-args r))]))

; Creates a list of parameters
(define (check-dup-helper [s : Sexp]) : (Listof Symbol)
  (match s
    [(cons (list (? symbol? id) ': type) r) (cons id (check-dup-helper r))]
    ['() '()]
    [other (error 'check-dup-helper "AAQZ : Invalid param syntax")]))

; Parse helper for params in LamC
(define (parse-param-types [s : Sexp]) : (Listof Type)
  (match s
    [(cons (list (? symbol? id) ': type) r) (cons (parse-type type) (parse-param-types r))]
    ['() '()]
    [other (error 'parse-param-types "AAQZ : Invalid param syntax")]))


; ----------------------------------- TYPE CHECKER FUNCTIONS -----------------------------------


; Parses an sexp into a Type AST
(define (parse-type [s : Sexp]) : Type
  (match s
    ['num (NumT)]
    ['bool (BoolT)]
    ['str (StrT)]
    [(list params ... '-> t2) (FunT
                               (map parse-type (cast params (Listof Sexp)))
                               (parse-type t2))]
    [other (error 'parse-type "AAQZ : Invalid type ~s" s)]))

; Checks the type 
(define (type-check [exp : ExprC] [tenv : Tenv]) : Type
  (match exp
    [(NumC n) (NumT)]
    [(StrC str) (StrT)]
    [(IdC sym) (lookup-type sym tenv)]
    [(AppC f a)
     (match (type-check f tenv)
       [(FunT in out)
        (cond
          [(funt-check in (map (lambda ([e : ExprC]) (type-check e tenv)) a)) out]
          [else (error 'type-check "AAQZ : Invalid Function Types")])]
       [other (error 'type-check "AAQZ : Invalid Function Id Type")])]
    [(IfC x then else)
     (match (type-check x tenv)
       [(BoolT)
        (define then-type (type-check then tenv))
        (cond
          [(equal? then-type (type-check else tenv))
           then-type]
          [else (error 'type0-checker "AAQZ : then and else types don't match")])]
       [other (error 'type-check "AAQZ : Invalid conditional")])]
    [(LamC p t b)
     (FunT t (type-check b (type-extend-helper p t tenv)))]
    [(RecC f a at rt b u)
     (define tenv-extend (type-extend-helper (list f) (list rt) tenv))
     (cond
       [(equal? (type-check b tenv-extend) rt) (type-check u tenv-extend)]
       [else (error 'type-check "AAQZ : Invalid Recursive Function Types")])]))

; Finds binding-type in env given a symbol
(define (lookup-type [for : Symbol] [tenv : Tenv]) : Type
  (match tenv
    ['() (error 'lookup-type "AAQZ : type not found: ~e" for)]
    [(cons (Type-Binding name type) r)
     (cond
       [(symbol=? for name) type]
       [else (lookup-type for r)])]))

; Compares the FunT type with given type arguments
(define (funt-check [in : (Listof Type)] [arg : (Listof Type)]) : Boolean
  (cond
    [(equal? (length in) (length arg))
     (match* (in arg)
       [((cons f-in r-in) (cons f-arg r-arg))
        (cond
          [(equal? f-in f-arg) (funt-check r-in r-arg)]
          [else #f])]
       [('() '()) #t]
       )]
    [else (error 'funt-check "AAQZ : Wrong number of args")]))


; adds new type bindings to the environment
(define (type-extend-helper [args : (Listof Symbol)] [types : (Listof Type)] [tenv : Tenv]) : Tenv
  ; assume both lists are equal length
  (match* (args types)
    [((cons f-a r-a) (cons f-t r-t)) (type-extend-helper r-a r-t (extend-tenv (Type-Binding f-a f-t) tenv))]
    [('() '()) tenv]))


; ----------------------------------- STORE FUNCTIONS -----------------------------------


; Creates a Storage of given size
(define (make-initial-store [memsize : Natural]) : Store
  (define top-store : Store (make-vector memsize (NumV 0)))
  (cond
    [(<= memsize 1) (error 'make-initial-store "AAQZ : Ran out of memory")])
  (vector-set! top-store 0 (NumV 1))
  (top-storing
   (list (PrimV '+)
         (PrimV '-)
         (PrimV '*)
         (PrimV '/)
         (PrimV '<=)
         (PrimV 'num-eq?)
         (PrimV 'str-eq?)
         (BoolV #t)
         (BoolV #f))
   top-store)
  top-store)

; Stores all the PrimVs into the store
(define (top-storing [prims : (Listof Value)] [sto : Store]) : NumV
  (match prims
    [(cons f r) (store-set f sto) (top-storing r sto)]
    ['() (NumV 0)]))

; Sets value at next position in store, returns NullV
(define (store-set [val : Value] [sto : Store]) : NumV
  (define new-loc (get-new-loc sto))
  (cond
    [(<= new-loc (- (vector-length sto) 1))
     (vector-set! sto new-loc val) ; set new val to new loc
     (vector-set! sto 0 (NumV (+ new-loc 1))) ; update next location 
     (NumV 0)]
    [else (error 'top-store-set "AAQZ : Ran out of memory")]))

; Given a store, returns the next new location to store
(define (get-new-loc [sto : Store]) : Location
  (define new-loc (vector-ref sto 0))
  (match new-loc
    [(NumV (? natural? loc)) loc]))

; finds id in environment and changes the value in the store
(define (mutate [id : Symbol] [val : Value] [env : Env] [sto : Store]) : NumV
  (match env
    ['() (error 'mutate "AAQZ : name not found: ~e" id)]
    [(cons (Binding name loc) r)
     (cond
       [(symbol=? id name)
        
        ; Set value given location in store
        (vector-set! sto loc val)
        (NumV 0)]
       [else (mutate id val r sto)])]))



; ----------------------------------- INTERP FUNCTIONS -----------------------------------


; Turns value into string
(define (serialize [val : Value]) : String
  (match val
    [(NumV n) (format "~v" n)]
    [(StrV str) (format "~v" str)]
    [(PrimV op) "#<primop>"]
    [(BoolV b) (if b "true" "false")]
    [(CloV param body env) "#<procedure>"]))


; Interprets an ExprC given an environment and storage to evaluate into a Value
(define (interp [exp : ExprC] [env : Env] [sto : Store]) : Value
  (match exp
    [(NumC num) (NumV num)]
    [(StrC str) (StrV str)]
    [(IdC sym) (lookup sym env sto)]
    [(IfC x t e) (define boo (interp x env sto))
                 (match boo
                   [(BoolV bool) (cond [(equal? bool #t) (interp t env sto)]
                                       [else (interp e env sto)])])]
    [(RecC f a at rt b u)
     ; 1. create placeholder for f and 2. extend cur env 
     (define extended-env (extend-helper (list f) (list (NumV 0)) env sto))
     ; 3. interp E1 as V
     (define ResultV (interp b extended-env sto))
     ; 4. mutate function to V
     (mutate f ResultV extended-env sto)
     ; 5. interp E2 in extended env
     (interp u extended-env sto)]
    [(LamC p t b) (CloV p b env)]
    [(AppC f a) (define f-value (interp f env sto))
                (define val-lst (map (lambda ([arg : ExprC]) (interp arg env sto)) a))
                (match f-value
                  [(CloV p b e) (interp b (extend-helper p val-lst e sto) sto)]
                  [(PrimV sym) (interp-PrimV sym val-lst sto)])]))

; Finds binding in env given a symbol
(define (lookup [for : Symbol] [env : Env] [sto : Store]) : Value
  (match env
    ['() (error 'lookup "AAQZ : name not found: ~e" for)]
    [(cons (Binding name loc) r)
     (cond
       [(symbol=? for name)
        
        ; Lookup value given location in store
        (vector-ref sto loc)]
       [else (lookup for r sto)])]))


; adds new bindings to the environment while storing value to location in store
(define (extend-helper [args : (Listof Symbol)] [vals : (Listof Value)] [env : Env] [sto : Store]) : Env
  ; assume both lists are equal length
  (match* (args vals)
    [((cons f-a r-a) (cons f-v r-v))
     (define new-loc (get-new-loc sto))
     ; Adds new value to location in store
     (store-set f-v sto)

     ; Adds binding of name and location to the environment
     (extend-helper r-a r-v (extend-env (Binding f-a new-loc) env) sto)]
    [('() '()) env]))

; helper for PrimV
(define (interp-PrimV [sym : Symbol] [val-lst : (Listof Value)] [sto : Store]) : Value
  (match* (sym val-lst)
    [('+ (list (NumV l) (NumV r))) (NumV (+ l r))]
    [('- (list (NumV l) (NumV r))) (NumV (- l r))]
    [('* (list (NumV l) (NumV r))) (NumV (* l r))]
    [('/ (list (NumV l) (NumV r))) (cond
                                     [(zero? r) (error 'interp-PrimV "AAQZ : Cannot divide by 0")]
                                     [else (NumV (/ l r))])]
    [('<= (list (NumV l) (NumV r))) (BoolV (<= l r))]
    [('num-eq? (list (NumV l) (NumV r))) (BoolV (equal? l r))]
    [('str-eq? (list (StrV l) (StrV r))) (BoolV (equal? l r))]))


; Combines parsing and evaluation; hard-core memory size of 2000
(define (top-interp [s : Sexp]) : String
  (define parsed (parse s))
  (type-check parsed base-tenv)
  (serialize (interp parsed top-env (make-initial-store 2000))))


; ----------------------------------------------------------------------------------
; ----------------------------------- TEST CASES -----------------------------------
; ----------------------------------------------------------------------------------


; ----------------------------------- TEST PARSING FUNCTIONS -----------------------------------

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
(check-equal? (parse '(+ 1)) (AppC (IdC '+) (list (NumC 1)))) ; pass parse, fail interp
(check-equal? (parse '({[x : num]} => 1)) (LamC (list 'x) (list (NumT)) (NumC 1)))
(check-equal? (parse '({[x : num]} => x)) (LamC (list 'x) (list (NumT)) (IdC 'x)))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function ((x : num) (x : num))"))
           (lambda () (parse '({[x : num] [x : num]} => (+ x 1)))))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function ((x : num) (x : str))"))
           (lambda () (parse '({[x : num] [x : str]} => (+ x 1)))))
(check-equal? (parse '({[x : num] [y : num]} => x)) (LamC (list 'x 'y) (list (NumT) (NumT)) (IdC 'x)))
(check-equal? (parse '{{[x : num]} => +}) (LamC (list 'x) (list (NumT)) (IdC '+)))
(check-equal? (parse '{{} => 0}) (LamC '() '() (NumC 0)))
(check-equal? (parse '{{[+ : str]} => 0}) (LamC (list '+) (list (StrT))(NumC 0))) ; valid in racket and AAQZ4
(check-equal? (parse '(bind [x : num = 1] {+ 1 x}))
              (AppC
               (LamC (list 'x) (list (NumT)) (AppC (IdC '+) (list (NumC 1) (IdC 'x))))
               (list (NumC 1))))
(check-equal? (parse '(bind [x : num = 1] [y : num = 2] {+ x y}))
              (AppC
               (LamC (list 'x 'y) (list (NumT) (NumT)) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
               (list (NumC 1) (NumC 2))))
(check-equal? (parse '(bind [x : {num -> num} = {+ 1 2}] [y : num = 2] {+ x y}))
              (AppC
               (LamC (list 'x 'y) (list (FunT (list (NumT)) (NumT)) (NumT))
                     (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
               (list (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 2))))

(check-equal? (parse '{recbind [square-helper
                                : {num -> num}
                                = {([n : num]) => {if {<= n 0} 0 {+ n {square-helper {- n 2}}}}}]
                               {bind [square
                                      : {num -> num}
                                      = {([n : num]) => {square-helper {- {* 2 n} 1}}}]
                                     {square 13}}})
              (RecC 'square-helper
                    (list 'n)
                    (list (NumT))
                    (FunT (list (NumT)) (NumT))
                    (LamC (list 'n)
                          (list (NumT))
                          (IfC (AppC (IdC '<=) (list (IdC 'n) (NumC 0)))
                               (NumC 0)
                               (AppC (IdC '+)
                                     (list (IdC 'n)
                                           (AppC (IdC 'square-helper)
                                                 (list (AppC (IdC '-)
                                                             (list (IdC 'n) (NumC 2)))))))))
                    (AppC (LamC (list 'square)
                                (list (FunT (list (NumT)) (NumT)))
                                (AppC (IdC 'square) (list (NumC 13))))
                          (list (LamC (list 'n)
                                      (list (NumT))
                                      (AppC (IdC 'square-helper)
                                            (list (AppC (IdC '-)
                                                        (list (AppC (IdC '*)
                                                                    (list (NumC 2) (IdC 'n)))
                                                              (NumC 1))))))))))



(check-exn (regexp (regexp-quote "AAQZ : recbind invalid function definition"))
           (lambda () (parse '(recbind [fact : {num -> num} = {+ 1 2}] 1))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id 'bind"))
           (lambda () (parse '(bind [bind : num = 1] bind))))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function (x x)"))
           (lambda () (parse '(bind [x : num = 1] [x : num = 2] {+ 1 x}))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression #t"))
           (lambda () (parse #t)))



; extract-symbols
(check-equal? (extract-symbols '() '()) '())
(check-equal? 
 (extract-symbols '((x : num = (+ 1 2)) (y : num = (* 3 4))) '()) 
 '(x y))
(check-equal? (extract-symbols '((x : num = 2) (y : num = 3)) '()) 
              '(x y))
(check-equal? (extract-symbols '((x : num = 42)) '()) 
              '(x))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id 'bind"))
           (lambda () (extract-symbols '((bind : num = 42)) '())))
(check-exn (regexp (regexp-quote "AAQZ : Invalid binding clause structure: (bind this is wrong not valid)"))
           (lambda () (extract-symbols '(bind this is wrong not valid) '())))

; extract-expressions
(check-equal? (extract-expressions '() '()) '())
(check-equal? 
 (extract-expressions '((x : num = (+ 1 2)) (y : num = (* 3 4))) '()) 
 (list (AppC (IdC '+) (list (NumC 1) (NumC 2))) (AppC (IdC '*) (list (NumC 3) (NumC 4)))))
(check-equal? (extract-expressions '((x : num = 2) (y : num = 3)) '()) 
              (list (NumC 2) (NumC 3)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid binding clause structure: (bind this is wrong not valid)"))
           (lambda () (extract-expressions '(bind this is wrong not valid) '())))

; valid-id?
(check-equal? (valid-id? 'test) #t)
(check-equal? (valid-id? '+) #t)
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id 'if"))
           (lambda () (valid-id? 'if)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id '=>"))
           (lambda () (valid-id? '=>)))

; valid-args
(check-equal? (valid-args '(a b c d e)) (list 'a 'b 'c 'd 'e))


; ----------------------------------- TEST type-check FUNCTIONS -----------------------------------

; parse-type
(check-equal? (parse-type 'num) (NumT))
(check-equal? (parse-type 'bool) (BoolT))
(check-equal? (parse-type 'str) (StrT))
(check-equal? (parse-type '{num -> num}) (FunT (list (NumT)) (NumT)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid type weird"))
           (lambda () (parse-type 'weird)))

; lookup-type
(check-equal? (lookup-type '+ base-tenv) (FunT (list (NumT) (NumT)) (NumT)))
(check-exn (regexp (regexp-quote "AAQZ : type not found: 'test"))
           (lambda () (lookup-type 'test base-tenv)))

; extract-types
(check-equal? (extract-types '() '()) '())
(check-equal? 
 (extract-types '((x : num = (+ 1 2)) (y : num = (* 3 4))) '()) 
 (list (NumT) (NumT)))
(check-equal? (extract-types '((x : num = 2) (y : num = 3)) '()) 
              (list (NumT) (NumT)))
(check-equal? (extract-types '((x : {num -> num} = {{[x : num]} => {+ x 1}})) '()) 
              (list (FunT (list (NumT)) (NumT))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid type ds"))
           (lambda () (extract-types '((x : {ds -> num} = {{[x : num]} => {+ x 1}})) '())))
(check-equal? (extract-types '((x : {num str -> num} = {{[x : num] [y : str]} => {+ x 1}})) '()) 
              (list (FunT (list (NumT) (StrT)) (NumT))))
(check-equal? (extract-types '((x : {num -> num} = {{[x : num]} => {+ x 1}})
                               (y : {str -> str} = {{[x : str]} => x})) '()) 
              (list (FunT (list (NumT)) (NumT)) (FunT (list (StrT)) (StrT))))
(check-equal? (extract-types '((x : num = 42)) '()) 
              (list (NumT)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid binding clause structure: (bind this is wrong not valid)"))
           (lambda () (extract-types '(bind this is wrong not valid) '())))
(check-exn (regexp (regexp-quote "AAQZ : Invalid binding clause structure: ((5 : num = 5))"))
           (lambda () (extract-types '((5 : num = 5)) '())))

; check-dup-helper
(check-equal? (check-dup-helper '()) '())
(check-equal? (check-dup-helper (list '[x : num])) (list 'x))
(check-equal? (check-dup-helper (list '[x : num] '[y : str])) (list 'x 'y))
(check-exn (regexp (regexp-quote "AAQZ : Invalid param syntax"))
           (lambda () (check-dup-helper (list '[x = wrong]))))

; parse-param-types
(check-equal? (parse-param-types '()) '())
(check-equal? (parse-param-types '([x : num])) (list (NumT)))
(check-equal? (parse-param-types '([x : num] [y : str])) (list (NumT) (StrT)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid param syntax"))
           (lambda () (parse-param-types '([x = wrong]))))

; funt-check
(check-equal? (funt-check (list (NumT)) (list (NumT))) #t)
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of args"))
           (lambda () (funt-check (list (NumT)) '())))

; ----------------------------------- TEST STORE FUNCTIONS -----------------------------------


; make-initial-store
(check-equal? (make-initial-store 13)
              (vector
               (NumV 10)
               (PrimV '+)
               (PrimV '-)
               (PrimV '*)
               (PrimV '/)
               (PrimV '<=)
               (PrimV 'num-eq?)
               (PrimV 'str-eq?)
               (BoolV #t)
               (BoolV #f)
               (NumV 0)
               (NumV 0)
               (NumV 0)))
(check-exn (regexp (regexp-quote "AAQZ : Ran out of memory"))
           (lambda () (make-initial-store 0)))

; store-set
(check-equal? (store-set (NumV 10) (make-initial-store 20)) (NumV 0))
(check-exn (regexp (regexp-quote "AAQZ : Ran out of memory"))
           (lambda () (store-set (NumV 10) (make-initial-store 3))))

; mutate
(check-equal? (mutate 'x (NumV 5)
                      (list
                       (Binding 'x 1))
                      (vector
                       (NumV 2)
                       (NumV 3))) (NumV 0))
(check-exn (regexp (regexp-quote "AAQZ : name not found: 'x"))
           (lambda () (mutate 'x (NumV 5)
                              (list
                               (Binding 'y 1))
                              (vector
                               (NumV 2)
                               (NumV 3)))))


; ----------------------------------- TEST INTERP FUNCTIONS -----------------------------------

; lookup
(check-equal? (lookup 'test (list (Binding 'test 1)) (vector
                                                      (NumV 2)
                                                      (NumV 1))) (NumV 1))
(check-equal? (lookup 'test (list (Binding 'test1 1)
                                  (Binding 'test2 2)
                                  (Binding 'test 3))
                      (vector
                       (NumV 3)
                       (NumV 1)
                       (NumV 2)
                       (NumV 3))) (NumV 3))

(check-exn (regexp (regexp-quote "AAQZ : name not found: 'test"))
           (lambda () (lookup 'test (list (Binding 'failtest 1))
                              (vector
                               (NumV 1)
                               (NumV 1))))) 
; serialize
(check-equal? (serialize (NumV 023948)) "23948")
(check-equal? (serialize (PrimV '+)) "#<primop>")
(check-equal? (serialize (StrV "testing")) "\"testing\"")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (CloV (list 'x) (IdC 'x) top-env)) "#<procedure>")
(check-equal? (serialize (CloV (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))) top-env)) "#<procedure>")


; interp

; top-interp
(check-equal? (top-interp 3) "3")
(check-equal? (top-interp "test") "\"test\"")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp '(if (<= 0 1) 0 1)) "0")
(check-equal? (top-interp '(if (<= 1 0) 0 1)) "1")
(check-equal? (top-interp '(+ 1 2)) "3")
(check-equal? (top-interp '(/ 4 2)) "2")
(check-equal? (top-interp '((([a : num]) => 1) 1)) "1")
(check-equal? (top-interp '((([a : num]) => a) 1)) "1")
(check-equal? (top-interp '((([a : num] [b : num]) => (+ a b)) 1 2)) "3")
(check-equal? (top-interp '((([a : num] [b : num]) => (- a b)) 3 2)) "1")
(check-equal? (top-interp '((([a : num] [b : num]) => (if (<= a b)
                                                          "testing is true" "b is smaller")) 1 2))
              "\"testing is true\"")
(check-equal? (top-interp '((([a : num] [b : num]) => (if (<= a b)
                                                          "testing is true" "b is smaller")) 2 1))
              "\"b is smaller\"")
(check-equal? (top-interp '((([x : num] [y : num]) => (if (num-eq? x y)
                                                          "match" "no match")) 3 3))
              "\"match\"")
(check-equal? (top-interp '((([x : num] [y : num]) => (if (num-eq? x y)
                                                          "match" "no match")) 3 4))
              "\"no match\"")
(check-equal? (top-interp '((([x : str] [y : str]) => (if (str-eq? x y)
                                                          "match" "no match")) "test" "test"))
              "\"match\"")
(check-equal? (top-interp '((([x : str] [y : str]) => (if (str-eq? x y)
                                                          "match" "no match")) "test" "wrong"))
              "\"no match\"")
(check-equal? (top-interp '((([x : num]) => (* x x)) 4)) "16")
(check-equal? (top-interp '((([x : num]) => ((([y : num]) => (+ x y)) 10)) 5)) "15")


#; (check-exn (regexp (regexp-quote "AAQZ : name not found: 'fact"))
              (lambda () (top-interp '(bind [fact : {num -> num} =
                                                  (([n : num]) => {if {<= n 0} 1 {* n {fact {- n 1}}}})]
                                            (fact 4)))))
#; (check-equal? (top-interp '(((f g) => (((x) => (g (f x))) 2))
                               ((x) => (+ x 3))
                               ((y) => (* y 2)))) "10")
#; (check-equal? (top-interp '(bind [one = {{f} => {{a} => {f a}}}]
                                    ((one {{x} => {+ x 1}}) 1))) "2")
#; (check-equal? (top-interp '(bind [two = {{f} => {{a} => {f {f a}}}}]
                                    ((two {{x} => {+ x 1}}) 1))) "3")
#; (check-equal? (top-interp '(bind [add = {{n1} => {{n2} => {{f} => {{a} => {{n1 f} {{n2 f} a}}}}}}]
                                    ((((add {{f} => {{a} => {f a}}})
                                       {{f} => {{a} => {f {f a}}}})
                                      {{x} => {+ x 1}}) 1))) "4")

(check-equal? (top-interp '{recbind [square-helper
                                     : {num -> num}
                                     = {([n : num]) => {if {<= n 0} 0 {+ n {square-helper {- n 2}}}}}]
                                    {bind [square
                                           : {num -> num}
                                           = {([n : num]) => {square-helper {- {* 2 n} 1}}}]
                                          {square 13}}})
              "169")
(check-equal? (top-interp '{recbind [tri : {num -> num} =  {([x : num]) => {if {num-eq? x 0}
                                                                               0
                                                                               {+ x {tri {- x 1}}}}}]
                                    {tri 6}}) "21")

; errors
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function"))
           (lambda () (top-interp '(1 2 3 4))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function Types"))
           (lambda () (top-interp '((([x : num] [y : num]) => (+ x y)) 1 "1"))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function Types"))
           (lambda () (top-interp '((([x : num] [y : num]) => (- x y)) 1 "1"))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function Types"))
           (lambda () (top-interp '((([x : num] [y : num]) => (* x y)) 1 "1"))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function Types"))
           (lambda () (top-interp '((([x : num] [y : num]) => (/ x y)) 1 "1"))))
(check-exn (regexp (regexp-quote "AAQZ : Cannot divide by 0"))
           (lambda () (top-interp '((([x : num] [y : num]) => (/ x y)) 1 0))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function Types"))
           (lambda () (top-interp '((([x : num] [y : num]) => (<= x y)) 1 "1"))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function Types"))
           (lambda () (top-interp '{bind [add1 : {num -> num} =
                                               {{[x : str]} => {+ x 1}}]
                                         {add1 3}})))
(check-exn (regexp (regexp-quote "AAQZ : Invalid conditional"))
           (lambda () (top-interp '{if {+ 1 2} 3 4})))
(check-exn (regexp (regexp-quote "AAQZ : then and else types don't match"))
           (lambda () (top-interp '{if true 3 "hello"})))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Function Id Type"))
           (lambda () (top-interp '{recbind [square-helper
                                             : num
                                             = {([n : num]) => {if {<= n 0} 0 {+ n {square-helper {- n 2}}}}}]
                                            {bind [square
                                                   : {num -> num}
                                                   = {([n : num]) => {square-helper {- {* 2 n} 1}}}]
                                                  {square 13}}})))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Recursive Function Types"))
           (lambda () (top-interp '{recbind [square-helper
                                             : {num -> num}
                                             = {([n : num]
                                                 [x : str])
                                                => {if {<= n 0} 0 {+ n {square-helper {- n 2}}}}}]
                                            {bind [square
                                                   : {num -> num}
                                                   = {([n : num]) => {square-helper {- {* 2 n} 1}}}]
                                                  {square 13}}})))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of args"))
           (lambda () (top-interp '{+ 1 2 3})))

