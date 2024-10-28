#lang racket

(require typed/rackunit)

; takes a function of 1 argument and calls it once
(define one (λ (f) (λ (a) (f a))))

; takes a function of 1 argument and calls it twice
(define two (λ (f) (λ (a) (f (f a)))))

; takes a function of 1 argument and calls its argument
(define zero (λ (f) (λ (a)  a)))

; takes a function of 1 argument and calls it n+1 times
(define add1 (λ (n) (λ (f) (λ (a) (f ((n f) a))))))

; takes a function of 1 argument and calls it n1+n2 times
(define add (λ (n1) (λ (n2) (λ (f) (λ (a) ((n1 f) ((n2 f) a)))))))

; takes 2 arguments and returns the first
(define tru (λ (a1) (λ (a2) a1)))

; takes 2 arguments and returns the second
(define fals (λ (a1) (λ (a2) a2)))

; calls a bool on 2 arguments. 
(define if (λ (b) (λ (a1) (λ (a2) ((b a1) a2)))))

 ;; this code defines the sim-AAQV4 language as a module
  (module sim-AAQZ4 racket
    (provide
     [rename-out (#%lam-app #%app)
                 (my-if if)]
     else
     #%module-begin
     #%datum
     + - * / = equal? <= =>
     true false
     bind)
    
    (require (for-syntax syntax/parse))
   
    (define-syntax (#%lam-app stx)
      (syntax-case stx (=>)
        [(_ (args ...) => body)
         #'(lambda (args ...) body)]
        [(_ e ...)
         #'(#%app e ...)]))
   
    ;; leaving it in just to have it here for next time:
    (define-syntax (my-if stx)
      (syntax-case stx (:)
        [(_ e1 e2 e3)
         #'(if e1 e2 e3)]))
   
    (define-syntax (bind stx)
      (syntax-parse stx
        [((~literal bind)
          (var:id (~literal =) rhs) ...
          body:expr)
         #'(let ([var rhs] ...) body)])))
   
  ;; this module uses the sim-AAQZ4 language. That is, its
  ;; contents are written *in* the sim-AAQZ4 language.
  ;; the only edits you should make are inside this module:
  (module my-mod1 (submod ".." sim-AAQZ4)
   
    1234
   
    4567
   
    {+ 4 5}
   
    {if true 34 39}
    
    {{(x y) => {+ x y}} 4 3}
   
    {bind [z = {+ 9 14}]
          [y = 98]
          {+ z y}}
   
   
    ;; exercise 0: Using the binding form, give the name
    ;; `f` to the function that accepts an argument `x` and computes
    ;; x^2 + 4x + 4. Apply `f` to seven.

    (bind [f = {{x} => {+ {+ {* x x} {* 4 x}} 4}}] (f 1))
    ;; exercise 1: Use the trick discussed in class to define
    ;; a `fact` function that computes the factorial of a given
    ;; number. Use it to compute the factorial of 12.
    (bind [fact = {{self n} => {if {<= n 1} 1 {* n {self self {- n 1}}}}}] (fact fact 12))
   
    ;; exercise 2: Define a 'pow' function that accepts a base
    ;; and a (natural number) exponent, and computes the base to
    ;; the power of the exponent. Don't worry about non-natural
    ;; number exponents (6^1.5, 5^-4).
    (bind [pow = {{self base exp} =>
                                  {if {<= exp 0} 1 {* base
                                                      {self self base {- exp 1}}}}}]
          (pow pow 3 3))
   
    ;; exercise 3: use `fact` and `pow` to build a "sin" function
    ;; that accepts a number x and a number of terms `n`, and computes
    ;; (sin x) using `n` terms of the taylor expansion. (Note that
    ;; this is a little ambigious; do zero-coefficient terms count?
    ;; You can go either way on this.) Use this function to compute
    ;; the sine of 1 radian to an error of no more than ten to the minus
    ;; 30th.
    ; sin(x)= x - x^3/3! +  x^5/5! -x^7/7!
    (bind 
     [sin = 
          {{self x n} => {if {<= n 0} 0
                             {+
                              {/ (bind [pow = {{self base exp} =>
                                                               {if {<= exp 0} 1 {* base {self self base {- exp 1}}}}}]
                                       {pow pow {* x (pow pow -1 (+ n 1))} {- {* 2 n} 1}})
                                 (bind [fact = {{self n} => {if {<= n 1} 1 {* n {self self {- n 1}}}}}]
                                       {fact fact {- {* 2 n} 1}})}
                              {self self x {- n 1}}}}}]
     (sin sin (/ 3.14159265 6) 100))



    ;; Lab5

    ; one
    (bind [one = {{f} => {{a} => {f a}}}]
           ((one {{x} => {+ x 1}}) 1))

    ; two
    (bind [two = {{f} => {{a} => {f {f a}}}}]
           ((two {{x} => {+ x 1}}) 1))

    ; add
    (bind [add = {{n1} => {{n2} => {{f} => {{a} => {{n1 f} {{n2 f} a}}}}}}]
          ((((add {{f} => {{a} => {f a}}})
               {{f} => {{a} => {f {f a}}}})
           {{x} => {+ x 1}}) 1))

    )
   
  ;; this code actually invokes the 'my-mod1 module, so that the
  ;; code runs.
  (require 'my-mod1)

; tests
(define (tplus1 a) (+ a 1))

; one
(check-equal? ((one tplus1) 1) 2)

; two
(check-equal? ((two tplus1) 1) 3)

; zero
(check-equal? ((zero tplus1) 1) 1)

; add1
(check-equal? (((add1 zero) tplus1) 1) 2)
(check-equal? (((add1 one) tplus1) 1) 3)
(check-equal? (((add1 two) tplus1) 1) 4)

; add
(check-equal? ((((add zero) one) tplus1) 1) 2)
(check-equal? ((((add one) two) tplus1) 1) 4)
(check-equal? ((((add two) one) tplus1) 1) 4)
(check-equal? ((((add two) two) tplus1) 1) 5)

; tru
(check-equal? ((tru 1) 2) 1)
(check-equal? ((tru 3) 4) 3)

; fals
(check-equal? ((fals 1) 2) 2)
(check-equal? ((fals 3) 4) 4)

; if
(check-equal? (((if tru) 1) 2) 1)
(check-equal? (((if fals) 1) 2) 2)


