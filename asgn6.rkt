#lang typed/racket

; Completed and passed all test cases! :D

(require typed/rackunit)

(define-type ExprC (U NumC IdC StrC IfC AppC LamC MutC))
(struct NumC ([num : Real]) #:transparent) ; EXPR = num
(struct MutC ([id : ExprC] [val : ExprC]) #:transparent)
(struct IdC ([sym : Symbol]) #:transparent)
(struct StrC ([str : String]) #:transparent)
(struct IfC ([x : ExprC] [then : ExprC] [else : ExprC]) #:transparent) ; do x if then is true, else else
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ((param : (Listof Symbol)) [body : ExprC]) #:transparent) ; Functions

; Values
(define-type Value (U NumV PrimV CloV BoolV StrV NullV ArrV))
(struct NumV ([num : Real]) #:transparent)
(struct PrimV ([sym : Symbol]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct CloV ([param : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct BoolV ([bool : Boolean]) #:transparent)
(struct NullV ([n : Null]) #:transparent)
(struct ArrV ([size : Natural] [start : Location]) #:transparent)

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
        (Binding 'equal? 6)
        (Binding 'true 7)
        (Binding 'false 8)
        (Binding 'null 9)
        (Binding 'seq 10)
        (Binding 'make-array 11)
        (Binding 'array 12)
        (Binding 'aref 13)
        (Binding 'aset! 14)
        (Binding 'substring 15)
        (Binding 'error 16)))

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

    ; MutC
    [(list id ':= exp) (MutC (parse id) (parse exp))]

    ; StrC
    [(? string? str) (StrC str)]

    ; IfC
    [(list 'if x then else) (IfC (parse x) (parse then) (parse else))]

    ; Bind
    [(list 'bind clauses ... body)
     (define symbols (extract-symbols (cast clauses Sexp) '()))
     (define exprs (extract-expressions (cast clauses Sexp) '()))
     (if (equal? (length symbols) (length (remove-duplicates symbols)))
         (AppC (LamC symbols (parse body)) exprs)
         (error 'parse "AAQZ : Duplicate Arguments in Function ~a" symbols))]

    
    ; LamC
    [(list (list (? symbol? param) ...) '=> body)
     (cond [(check-duplicates param) (error 'parse "AAQZ : Duplicate Arguments in Function ~a" param)]
           [else (LamC (valid-args (cast param (Listof Symbol))) (parse body))])]

    ; AppC
    [(list fun param ...) (AppC (parse fun) (map parse param))]

    ; IdC
    [(? symbol? sym) (valid-id? sym) (IdC sym)]
    
    [other (error 'parse "AAQZ : Invalid S-Expression ~e" other)]))

;; Helper for desugaring that extracts symbols for LamC's parameters
(define (extract-symbols [binding-clauses : Sexp] [accumulated-ids : (Listof Symbol)]) : (Listof Symbol)
  (match binding-clauses
    [(list (list (? symbol? ids) '= expr))
     (valid-id? ids) (extract-symbols '() (cons ids accumulated-ids))]
    [(list (list (? symbol? ids) '= expr) binding-clauses ...)
     (extract-symbols binding-clauses (cons ids accumulated-ids))]
    ['() (reverse accumulated-ids)]
    [other (error 'extract-symbols "AAQZ : Invalid binding clause structure: ~a" binding-clauses)]))

(define (extract-expressions [clauses : Sexp] [accumulated-exprs : (Listof ExprC)]) : (Listof ExprC)
  (match clauses
    [(list (list (? symbol? id) '= exp)) (extract-expressions '() (cons (parse exp) accumulated-exprs))]
    [(list (list (? symbol? id) '= exp) clauses ...) (extract-expressions clauses (cons (parse exp) accumulated-exprs))]
    ['() (reverse accumulated-exprs)]
    [other (error 'extract-expressions "AAQZ : Invalid binding clause structure: ~a" clauses)]))

; Checks if the given symbol is a valid identifier
(define (valid-id? [sym : Any]) : Boolean
  (match sym
    [(or 'if '=> 'bind ':= '=) (error 'valid-id? "AAQZ : Invalid Id ~e" sym)]
    [else #t]))

; Checks if the list of symbols are valid-ids, and returns a list of IdC
(define (valid-args [syms : (Listof Symbol)]) : (Listof Symbol)
  (match syms
    [(list) '()]
    [(cons (? valid-id? id) r) (cons id (valid-args r))]))


; ----------------------------------- STORE FUNCTIONS -----------------------------------


; Creates a Storage of given size
(define (make-initial-store [memsize : Natural]) : Store
  (define top-store : Store (make-vector memsize (NullV null)))
  (cond
    [(<= memsize 1) (error 'make-initial-store "AAQZ : Ran out of memory")])
  (vector-set! top-store 0 (NumV 1))
  (top-storing
   (list (PrimV '+)
         (PrimV '-)
         (PrimV '*)
         (PrimV '/)
         (PrimV '<=)
         (PrimV 'equal?)
         (BoolV #t)
         (BoolV #f)
         (NullV null)
         (PrimV 'seq)
         (PrimV 'make-array)
         (PrimV 'array)
         (PrimV 'aref)
         (PrimV 'aset!)
         (PrimV 'substring)
         (PrimV 'error))
   top-store)
  top-store)

; Stores all the PrimVs into the store
(define (top-storing [prims : (Listof Value)] [sto : Store]) : NullV
  (match prims
    [(cons f r) (store-set f sto) (top-storing r sto)]
    ['() (NullV null)]))

; Sets value at next position in store, returns NullV
(define (store-set [val : Value] [sto : Store]) : NullV
  (define new-loc (get-new-loc sto))
  (cond
    [(<= new-loc (- (vector-length sto) 1))
     (vector-set! sto new-loc val) ; set new val to new loc
     (vector-set! sto 0 (NumV (+ new-loc 1))) ; update next location 
     (NullV null)]
    [else (error 'top-store-set "AAQZ : Ran out of memory")]))

; Given a store, returns the next new location to store
(define (get-new-loc [sto : Store]) : Location
  (define new-loc (vector-ref sto 0))
  (match new-loc
    [(NumV (? natural? loc)) loc]))

; finds id in environment and changes the value in the store
(define (mutate [id : Symbol] [val : Value] [env : Env] [sto : Store]) : NullV
  (match env
    ['() (error 'mutate "AAQZ : name not found: ~e" id)]
    [(cons (Binding name loc) r)
     (cond
       [(symbol=? id name)
        
        ; Set value given location in store
        (vector-set! sto loc val)
        (NullV null)]
       [else (mutate id val r sto)])]))

; Allocates memory in the store of given size filled with values of given lst
(define (allocate [size : Natural] [val-lst : (Listof Value)] [sto : Store]) : Location
  (cond
    [(and (equal? size (length val-lst)) (>= size 1))
     (define start (get-new-loc sto))
     (allocate-helper size val-lst sto)
     start]
    [else (error 'allocate "AAQZ : Wrong number of args")]))

(define (allocate-helper [size : Natural] [val-lst : (Listof Value)] [sto : Store]) : NullV
  (match val-lst
    [(cons val r)
     (cond
       [(<= size 0) (error 'allocate-helper "AAQZ : Wrong number of args")]
       [else (store-set val sto)
             (allocate-helper (- size 1) r sto)])]
    ['() (NullV null)]))


; ----------------------------------- INTERP FUNCTIONS -----------------------------------


; Turns value into string
(define (serialize [val : Value]) : String
  (match val
    [(NumV n) (format "~v" n)]
    [(StrV str) (format "~v" str)]
    [(PrimV op) "#<primop>"]
    [(BoolV b) (if b "true" "false")]
    [(CloV param body env) "#<procedure>"]
    [(NullV n) "null"]
    [(ArrV size start) "#<array>"]))


; Interprets an ExprC given an environment and storage to evaluate into a Value
(define (interp [exp : ExprC] [env : Env] [sto : Store]) : Value
  (match exp
    [(NumC num) (NumV num)]
    [(MutC id val)
     (match id
       [(IdC sym) (mutate sym (interp val env sto) env sto)]
       [other (error 'interp "AAQZ : Mutation wrong id reference")])]
    [(StrC str) (StrV str)]
    [(IdC sym) (lookup sym env sto)]
    [(IfC x t e) (define boo (interp x env sto))
                 (match boo
                   [(BoolV bool) (cond [(equal? bool #t) (interp t env sto)]
                                       [else (interp e env sto)])]
                   [other (error 'interp "AAQZ : Given non-boolean type")])]
    [(LamC p b) (CloV p b env)]
    [(AppC f a) (define f-value (interp f env sto))
                (define val-lst (map (lambda ([arg : ExprC]) (interp arg env sto)) a))
                (match f-value
                  [(CloV p b e)
                   (cond
                     [(equal? (length p) (length val-lst)) (interp b (extend-helper p val-lst e sto) sto)]
                     [else (error 'interp "AAQZ : Wrong number of args")])]
                  [(PrimV sym) (interp-PrimV sym val-lst sto)]
                  [other (error 'interp "AAQZ : closure or primitive in function application")])]))

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
    [('equal? (list l r))
     (cond [(and (and (not (CloV? l)) (not (PrimV? l))) (and (not (CloV? r)) (not (PrimV? r))))
            (BoolV (equal? l r))]
           [else (BoolV #f)])]
    [('seq (? list? lst-val)) (seq-helper lst-val)]
    [('make-array (list (NumV (? natural? n)) val)) (ArrV n (allocate n (make-list n val) sto))]
    [('array (list val ...))
     (define size (length val))
     (ArrV size (allocate size val sto))]
    [('aref (list (ArrV size loc) (NumV (? natural? idx))))
     (cond
       [(<= idx (- size 1)) (vector-ref sto (+ idx loc))]
       [else (error 'aref "AAQZ : Invalid index")])]
    [('aset! (list (ArrV size loc) (NumV (? natural? idx)) val))
     (cond
       [(<= idx (- size 1)) (vector-set! sto (+ idx loc) val) (NullV null)]
       [else (error 'aset! "AAQZ : Invalid index")])]
    [('substring (list (StrV str) (NumV (? natural? start)) (NumV (? natural? end))))
     (cond
       [(<= end (string-length str)) (StrV (substring str start end))]
       [else (error 'substring "AAQZ : Invalid substring")])]
    
    [('error (list val)) (error 'interp "AAQZ : user-error ~s" (serialize val))]
    [(_ _) (error 'interp-PrimV "AAQZ : Invalid PrimV")]))

; goes through the list of expressions and returns the last one.
(define (seq-helper [lst-val : (Listof Value)]) : Value
  (match lst-val
    [(cons f '()) f]
    [(cons f r) (seq-helper r)]))

; Combines parsing and evaluation
(define (top-interp [s : Sexp] [memsize : Natural]) : String
  (serialize (interp (parse s) top-env (make-initial-store memsize))))

; while
(define while "bogus")

; inorder
(define in-order '{{arr len} =>
                             {bind
                              [guard = {{len} =>
                                              {if {equal? len 1}
                                                  true
                                                  {seq
                                                   {len := {- len 1}}
                                                   false}}}]
                              {seq
                               {while := {{arr len} =>
                                                    {if
                                                     {equal? {guard len} false}
                                                     {if
                                                      {<= {aref arr {- len 1}}
                                                          {aref arr {- len 2}}}
                                                      false
                                                      {while arr {- len 1}}}
                                                     true}}}
                               {while arr len}}}})


; ----------------------------------------------------------------------------------
; ----------------------------------- TEST CASES -----------------------------------
; ----------------------------------------------------------------------------------

; ----------------------------------- TEST PARSING FUNCTIONS -----------------------------------

; parse
(check-equal? (parse '1) (NumC 1))
(check-equal? (parse 'x) (IdC 'x))
(check-equal? (parse 'y) (IdC 'y))
(check-equal? (parse '(x := 3)) (MutC (IdC 'x) (NumC 3)))
(check-equal? (parse "string") (StrC "string"))
(check-equal? (parse "string with spaces") (StrC "string with spaces"))
(check-equal? (parse '(if 1 3 0)) (IfC (NumC 1) (NumC 3) (NumC 0)))
(check-equal? (parse '(if (<= 1 2) 3 0))
              (IfC (AppC (IdC '<=) (list (NumC 1) (NumC 2))) (NumC 3) (NumC 0)))
(check-equal? (parse '(f 2)) (AppC (IdC 'f) (list (NumC 2))))
(check-equal? (parse '(g 3 4)) (AppC (IdC 'g) (list (NumC 3) (NumC 4))))
(check-equal? (parse '(+ 1 2)) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '(<= 1 2)) (AppC (IdC '<=) (list (NumC 1) (NumC 2))))
(check-equal? (parse '(+ 1)) (AppC (IdC '+) (list (NumC 1)))) ; valid in racket and AAQZ4; pass parse, fails interp
(check-equal? (parse '({x} => 1)) (LamC (list 'x) (NumC 1)))
(check-equal? (parse '({x} => x)) (LamC (list 'x) (IdC 'x)))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function (x x)"))
           (lambda () (parse '({x x} => (+ x 1)))))
(check-equal? (parse '({x y} => x)) (LamC (list 'x 'y) (IdC 'x)))
(check-equal? (parse '{{x} => +}) (LamC (list 'x) (IdC '+)))
(check-equal? (parse '{{} => 0}) (LamC '() (NumC 0)))
(check-equal? (parse '{{+} => 0}) (LamC (list '+) (NumC 0))) ; valid in racket and AAQZ4
(check-equal? (parse '(bind [x = 1] {+ 1 x}))
              (AppC
               (LamC (list 'x) (AppC (IdC '+) (list (NumC 1) (IdC 'x))))
               (list (NumC 1))))
(check-equal? (parse '(bind [x = 1] [y = 2] {+ x y}))
              (AppC
               (LamC (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
               (list (NumC 1) (NumC 2))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id 'bind"))
           (lambda () (parse '(bind [bind = 1] bind))))
(check-exn (regexp (regexp-quote "AAQZ : Duplicate Arguments in Function (x x)"))
           (lambda () (parse '(bind [x = 1] [x = 2] {+ 1 x}))))
(check-exn (regexp (regexp-quote "AAQZ : Invalid S-Expression #t"))
           (lambda () (parse #t)))


; extract symbols
(check-equal? (extract-symbols '() '()) '())
(check-equal? 
  (extract-symbols '((x = (+ 1 2)) (y = (* 3 4))) '()) 
  '(x y))
(check-equal? (extract-symbols '((x = 2) (y = 3)) '()) 
              '(x y))
(check-equal? (extract-symbols '((x = 42)) '()) 
              '(x))
(check-exn (regexp (regexp-quote "AAQZ : Invalid Id 'bind"))
           (lambda () (extract-symbols '((bind = 42)) '())))
(check-exn (regexp (regexp-quote "AAQZ : Invalid binding clause structure: (bind this is wrong not valid)"))
           (lambda () (extract-symbols '(bind this is wrong not valid) '())))

; extract-expressions
(check-equal? (extract-expressions '() '()) '())
(check-equal? 
  (extract-expressions '((x = (+ 1 2)) (y = (* 3 4))) '()) 
  (list (AppC (IdC '+) (list (NumC 1) (NumC 2))) (AppC (IdC '*) (list (NumC 3) (NumC 4)))))
(check-equal? (extract-expressions '((x = 2) (y = 3)) '()) 
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


; ----------------------------------- TEST STORE FUNCTIONS -----------------------------------


; make-initial-store
(check-equal? (make-initial-store 20)
              (vector
               (NumV 17)
               (PrimV '+)
               (PrimV '-)
               (PrimV '*)
               (PrimV '/)
               (PrimV '<=)
               (PrimV 'equal?)
               (BoolV #t)
               (BoolV #f)
               (NullV null)
               (PrimV 'seq)
               (PrimV 'make-array)
               (PrimV 'array)
               (PrimV 'aref)
               (PrimV 'aset!)
               (PrimV 'substring)
               (PrimV 'error)
               (NullV null)
               (NullV null)
               (NullV null)))
(check-exn (regexp (regexp-quote "AAQZ : Ran out of memory"))
           (lambda () (make-initial-store 1)))

; store-set
(check-equal? (store-set (NumV 10) (make-initial-store 20)) (NullV null))
(check-exn (regexp (regexp-quote "AAQZ : Ran out of memory"))
           (lambda () (store-set (NumV 10) (make-initial-store 17))))

; mutate
(check-equal? (mutate 'x (NumV 5)
                      (list
                       (Binding 'x 1))
                      (vector
                       (NumV 2)
                       (NumV 3))) (NullV null))
(check-exn (regexp (regexp-quote "AAQZ : name not found: 'x"))
           (lambda () (mutate 'x (NumV 5)
                              (list
                               (Binding 'y 1))
                              (vector
                               (NumV 2)
                               (NumV 3)))))

; allocate
(check-equal? (allocate 3
                        (list
                         (StrV "test")
                         (NumV 3)
                         (BoolV false))
                        (make-initial-store 20))
              17)
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of args"))
           (lambda () (allocate 0
                                (list
                                 (StrV "test")
                                 (NumV 3)
                                 (BoolV false))
                                (make-initial-store 20))))

; allocate-helper
(check-equal? (allocate-helper 3
                               (list
                                (StrV "test")
                                (NumV 3)
                                (BoolV false))
                               (make-initial-store 20))
              (NullV null))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of args"))
           (lambda () (allocate-helper 2
                               (list
                                (StrV "test")
                                (NumV 3)
                                (BoolV false))
                               (make-initial-store 20))))

; ----------------------------------- TEST INTERP FUNCTIONS -----------------------------------


(define top-store (make-initial-store 100))

; serialize
(check-equal? (serialize (NumV 023948)) "23948")
(check-equal? (serialize (PrimV '+)) "#<primop>")
(check-equal? (serialize (StrV "testing")) "\"testing\"")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (CloV (list 'x) (IdC 'x) top-env)) "#<procedure>")
(check-equal? (serialize (CloV (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))) top-env)) "#<procedure>")
(check-equal? (serialize (ArrV 3 4)) "#<array>")

; interp
(check-equal? (interp (NumC 1) top-env top-store) (NumV 1))
(check-equal? (interp (MutC (IdC 'x) (NumC 2))
                      (list
                       (Binding 'x 1))
                      (vector
                       (NumV 2)
                       (NumV 1))) (NullV null))
(check-exn (regexp (regexp-quote "AAQZ : Mutation wrong id reference"))
           (lambda () (interp (MutC (StrC "fail") (NumC 2))
                      (list
                       (Binding 'x 1))
                      (vector
                       (NumV 2)
                       (NumV 1)))))
(check-equal? (interp (StrC "test") top-env top-store) (StrV "test"))
(check-equal? (interp (IdC '+) top-env top-store) (PrimV '+))
(check-equal? (interp (IdC '-) top-env top-store) (PrimV '-))
(check-equal? (interp (IdC 'error) top-env top-store) (PrimV 'error))
(check-equal? (interp (IdC 'true) top-env top-store) (BoolV #t))
(check-equal? (interp (IdC 'false) top-env top-store) (BoolV #f))
(check-equal? (interp (IdC 'null) top-env top-store) (NullV null))
(check-exn (regexp (regexp-quote "AAQZ : name not found: 'x"))
           (lambda () (interp (IdC 'x) top-env top-store)))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (NumC 1) (NumC 2))) (NumC 3) (NumC 0)) top-env top-store)
                      (NumV 3))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (NumC 2) (NumC 1))) (NumC 3) (NumC 0)) top-env top-store)
                      (NumV 0))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (NumC 2) (NumC 1))) (IdC 'true) (IdC 'false)) top-env top-store)
                      (BoolV #f))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (NumC 2) (NumC 2))) (IdC 'true) (IdC 'false)) top-env top-store)
                      (BoolV #t))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 2)))
                           (IdC 'true) (StrC "False PrimV")) top-env top-store)
                      (StrV "False PrimV"))
(check-exn (regexp (regexp-quote "AAQZ : Given non-boolean type"))
           (lambda () (interp (IfC (StrC "string") (NumC 3) (NumC 0)) top-env top-store)))
(check-equal? (interp (LamC (list 'x) (NumC 1)) top-env top-store) (CloV (list 'x) (NumC 1) top-env))
(check-equal? (interp (LamC '() (StrC "Call with no args")) top-env top-store)
              (CloV '() (StrC "Call with no args") top-env))
(check-equal? (interp (AppC (IdC '+) (list (NumC 1) (NumC 0))) top-env top-store) (NumV 1))
(check-equal? (interp (AppC (IdC '-) (list (NumC 5) (NumC 3))) top-env top-store) (NumV 2))
(check-equal? (interp (AppC (IdC '+) (list (NumC 1) (NumC 0))) top-env top-store) (NumV 1))
(check-equal? (interp (AppC (LamC (list 'x) (IdC 'x)) (list (NumC 1))) top-env top-store) (NumV 1))
(check-equal? (interp (AppC (LamC (list 'x) (IdC 'x)) (list (NumC 1))) top-env top-store) (NumV 1))
(check-equal? (interp (AppC (LamC (list 'x 'y)
                                  (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
                            (list (NumC 1) (NumC 2))) top-env top-store)
              (NumV 3))
(check-equal? (interp (AppC (LamC (list '+ '-)
                                  (AppC (IdC '*) (list (IdC '+) (IdC '-))))
                            (list (NumC 5) (NumC 3))) top-env top-store)
              (NumV 15))
(check-equal? (interp (AppC (LamC (list 'x)
                                  (AppC (IdC '+) (list (IdC 'x) (NumC 1))))
                            (list (NumC 7))) top-env top-store)
              (NumV 8))
(check-exn (regexp (regexp-quote "AAQZ : Wrong number of args"))
           (lambda () (interp (AppC (LamC (list 'x) (NumC 1)) (list (NumC 1) (NumC 2))) top-env top-store)))

(check-exn (regexp (regexp-quote "AAQZ : user-error \"1\""))
           (lambda () (interp (AppC (IdC 'error) (list (NumC 1))) top-env top-store)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid PrimV"))
           (lambda () (interp (AppC (IdC '+) (list (NumC 1))) top-env top-store)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid PrimV"))
           (lambda () (interp (AppC (IdC 'error) (list (NumC 1) (NumC 2))) top-env top-store)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid PrimV"))
           (lambda () (interp (AppC (IdC 'error) (list (NumC 1) (NumC 2) (NumC 3))) top-env top-store)))


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

; interp-PrimV
(check-equal? (interp-PrimV '+ (list (NumV 1) (NumV 0)) top-store) (NumV 1))
(check-equal? (interp-PrimV '- (list (NumV 5) (NumV 3)) top-store) (NumV 2))
(check-equal? (interp-PrimV '* (list (NumV 5) (NumV 3)) top-store) (NumV 15))
(check-exn (regexp (regexp-quote "AAQZ : Invalid PrimV"))
           (lambda () (interp-PrimV '* (list (NumV 5)) top-store)))
(check-equal? (interp-PrimV '/ (list (NumV 15) (NumV 3)) top-store) (NumV 5))
(check-exn (regexp (regexp-quote "AAQZ : Cannot divide by 0"))
           (lambda () (interp-PrimV '/ (list (NumV 8329) (NumV 0)) top-store)))
(check-equal? (interp-PrimV '<= (list (NumV 5) (NumV 3)) top-store) (BoolV #f))
(check-equal? (interp-PrimV '<= (list (NumV 3) (NumV 5)) top-store) (BoolV #t))
(check-equal? (interp-PrimV 'equal? (list (NumV 3) (NumV 5)) top-store) (BoolV #f))
(check-equal? (interp-PrimV 'equal? (list (NumV 5) (NumV 5)) top-store) (BoolV #t))
(check-equal? (interp-PrimV 'equal? (list (StrV "test1") (StrV "test2")) top-store) (BoolV #f))
(check-equal? (interp-PrimV 'equal? (list (StrV "test1") (StrV "test1")) top-store) (BoolV #t))
(check-equal? (interp-PrimV 'equal? (list (PrimV '+) (StrV "test1")) top-store) (BoolV #f))
(check-equal? (interp-PrimV 'equal? (list (CloV '() (NumC 1) top-env) (NumV 1)) top-store) (BoolV #f))
(check-equal? (interp-PrimV 'make-array (list (NumV 3) (NumV 10)) (make-initial-store 20)) (ArrV 3 17))

(define test-store (make-initial-store 20))
(check-equal? (interp-PrimV 'array (list
                                    (StrV "test")
                                    (NumV 3)
                                    (BoolV false)) test-store) (ArrV 3 17))
(check-equal? (interp-PrimV 'aref (list (ArrV 3 17) (NumV 0)) test-store) (StrV "test"))
(check-equal? (interp-PrimV 'aref (list (ArrV 3 17) (NumV 1)) test-store) (NumV 3))
(check-equal? (interp-PrimV 'aref (list (ArrV 3 17) (NumV 2)) test-store) (BoolV #f))

(check-equal? (interp-PrimV 'substring (list (StrV "Apple") (NumV 1) (NumV 3)) top-store) (StrV "pp"))
(check-exn (regexp (regexp-quote "AAQZ : Invalid substring"))
           (lambda () (interp-PrimV 'substring (list (StrV "Apple") (NumV 1) (NumV 6)) top-store)))


; top-interp
(check-equal? (top-interp 3 100) "3")
(check-equal? (top-interp "test" 100) "\"test\"")
(check-equal? (top-interp 'true 100) "true")
(check-equal? (top-interp '(if (<= 0 1) 0 1) 100) "0")
(check-equal? (top-interp '(if (<= 1 0) 0 1) 100) "1")
(check-equal? (top-interp '(+ 1 2) 100) "3")
(check-equal? (top-interp '(((a) => 1) 1) 100) "1")
(check-equal? (top-interp '(((a) => a) 1) 100) "1")
(check-equal? (top-interp '(((a b) => (+ a b)) 1 2) 100) "3")
(check-equal? (top-interp '(((a b) => (- a b)) 3 2) 100) "1")
(check-equal? (top-interp '(((a b) => (equal? a b)) 3 2) 100) "false")
(check-equal? (top-interp '(((a b) => (if (<= a b) "testing is true" "b is smaller")) 1 2) 100) "\"testing is true\"")
(check-equal? (top-interp '(((a b) => (if (<= a b) "testing is true" "b is smaller")) 2 1) 100) "\"b is smaller\"")
(check-equal? (top-interp '(((x y) => (if (equal? x y) "match" "no match")) 3 3) 100) "\"match\"")
(check-equal? (top-interp '(((x y) => (if (equal? x y) "match" "no match")) 3 4) 100) "\"no match\"")
(check-equal? (top-interp '(((x) => (* x x)) 4) 100) "16")
(check-equal? (top-interp '(((x) => (((y) => (+ x y)) 10)) 5) 100) "15")
(check-exn (regexp (regexp-quote "AAQZ : name not found: 'fact"))
           (lambda () (top-interp '(bind [fact = ((n) => {if {<= n 0} 1 {* n {fact {- n 1}}}})] (fact 4)) 100)))
(check-equal? (top-interp '(bind
                            [fact = ((self n) => {if {<= n 0} 1 {* n {self self {- n 1}}}})]
                            (fact fact 4)) 100) "24")
(check-equal? (top-interp '(((f g) => (((x) => (g (f x))) 2))
                            ((x) => (+ x 3))
                            ((y) => (* y 2))) 100)
              "10")
(check-equal? (top-interp '(bind [one = {{f} => {{a} => {f a}}}]
                                 ((one {{x} => {+ x 1}}) 1)) 100)
              "2")
(check-equal? (top-interp '(bind [two = {{f} => {{a} => {f {f a}}}}]
                                 ((two {{x} => {+ x 1}}) 1)) 100)
              "3")
(check-equal? (top-interp '(bind [add = {{n1} => {{n2} => {{f} => {{a} => {{n1 f} {{n2 f} a}}}}}}]
                                 ((((add {{f} => {{a} => {f a}}})
                                    {{f} => {{a} => {f {f a}}}})
                                   {{x} => {+ x 1}}) 1)) 100)
              "4")
(check-equal? (top-interp '(equal? (substring (substring "abcd" 1 4) 1 3) "cd") 100) "true")

; array tests
(check-equal? (top-interp '(bind [test-lst = {make-array 3 10}]
                                 (aref test-lst 2)) 30) "10")
(check-equal? (top-interp '(bind [test-lst = {array 3 14 false 5}]
                                 (aref test-lst 2)) 30) "false")
(check-equal? (top-interp '(bind [test-lst = {array 3 14 false 5}]
                                 (aref test-lst 3)) 30) "5")
(check-equal? (top-interp '(bind [test-lst = {array 3 14 false 5}]
                                 (aset! test-lst 3 32)) 30) "null")
(check-equal? (top-interp '{bind [fact = "bogus"]
                                {seq {fact := {(x) => {if {equal? x 0}
                                                          1
                                                          {* x {fact {- x 1}}}}}}
                                     {fact 12}}} 100) "479001600")
(check-equal? (top-interp '{bind
                            [swap = {(arr i j) =>
                                               {bind [temp = {aref arr i}]
                                                     {seq
                                                      {aset! arr i {aref arr j}}
                                                      {aset! arr j temp}}}}]
                            {bind [my-arr = {array 10 20 30}]
                                  {seq
                                   {swap my-arr 0 2}
                                   {aref my-arr 2}}}} 100) "10")


; working-while
(define working-while '{bind 
                        [guard = {{len} =>
                                        {if {equal? len 1}
                                            true
                                            {seq
                                             {len := {- len 1}}
                                             false}}}]
                        [while = "bogus"]
                        {bind
                         [in-order = {{arr len} =>
                                                {while arr len}}]
                         {seq
                          {while := {{arr len} =>
                                               {if {equal? {guard len} false}
                                                   {if {<= {aref arr {- len 1}} {aref arr {- len 2}}}
                                                       false
                                                       {while arr {- len 1}}}
                                                   true}}}
                          {in-order (array 1 2 3 4 5) 5}}}})
(check-equal? (top-interp working-while 100) "true")

(define testing-while '{bind
                        [while = "bogus"]
                        {bind
                         [in-order =
                                   {{arr
                                     len}
                                    =>
                                    {bind
                                     [guard = {{len} =>
                                                     {if {equal? len 1}
                                                         true
                                                         {seq
                                                          {len := {- len 1}}
                                                          false}}}]
                                     {seq
                                      {while := {{arr
                                                  len}
                                                 =>
                                                 {if
                                                  {equal? {guard len} false}
                                                  {if
                                                   {<= {aref arr {- len 1}} {aref arr {- len 2}}}
                                                   false
                                                   {while arr {- len 1}}}
                                                  true}}}
                                      {while arr len}}}}]
                         {in-order (array 1 2 3 4 5) 5}}})
(check-equal? (top-interp testing-while 100) "true")

(define test-order (list 'bind
                    [list 'while '= while]
                    {list 'bind
                     [list 'in-order '= in-order]
                     '{if {in-order {array 3 6 8} 3}
                          1
                          0}}))
(check-equal? (top-interp  test-order 100) "1")

; errors
(check-exn (regexp (regexp-quote "AAQZ : user-error \"1\""))
           (lambda () (top-interp '(((x) => (error x)) 1) 100)))
(check-exn (regexp (regexp-quote "AAQZ : user-error \"8\""))
           (lambda () (top-interp '(((x) => (error x)) (((y) => (+ y 3)) 5)) 100)))
(check-exn (regexp (regexp-quote "AAQZ : user-error \"true\""))
           (lambda () (top-interp '(((x) => (error x))
                                    (((y) => (equal? y 5)) 5)) 100)))
(check-exn (regexp (regexp-quote "AAQZ : user-error \"false\""))
           (lambda () (top-interp '(((x) => (error x))
                                    (((y) => (equal? y 5)) 4)) 100)))
(check-exn (regexp (regexp-quote "AAQZ : closure or primitive in function application"))
           (lambda () (top-interp '(1 2 3 4) 100)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid PrimV"))
           (lambda () (top-interp '(((x y) => (+ x y)) 1 "1") 100)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid PrimV"))
           (lambda () (top-interp '(((x y) => (- x y)) 1 "1") 100)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid PrimV"))
           (lambda () (top-interp '(((x y) => (* x y)) 1 "1") 100)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid PrimV"))
           (lambda () (top-interp '(((x y) => (/ x y)) 1 "1") 100)))
(check-exn (regexp (regexp-quote "AAQZ : Cannot divide by 0"))
           (lambda () (top-interp '(((x y) => (/ x y)) 1 0) 100)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid PrimV"))
           (lambda () (top-interp '(((x y) => (<= x y)) 1 "1") 100)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid index"))
           (lambda () (top-interp '(bind [f = {make-array 5 false}]
                                 {aref f 5}) 100)))
(check-exn (regexp (regexp-quote "AAQZ : Invalid index"))
           (lambda () (top-interp '(bind [f = {make-array 5 false}]
                                 {aset! f 5 19}) 100)))


