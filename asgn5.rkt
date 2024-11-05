#lang typed/racket


#|
Everything Works! Play Blackjack


Fun Codes:
Blackjack: 900
Hit for a 21 (h s): 5
Get a 20, but dealer gets Blackjack: 7
Testing Tie (h s): 34 
Always losing: 4 (bad omen too)
|#

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
        (Binding 'error (PrimV 'error))
        (Binding 'println (PrimV 'println))
        (Binding 'read-num (PrimV 'read-num))
        (Binding 'read-str (PrimV 'read-str))
        (Binding 'seq (PrimV 'seq))
        (Binding '++ (PrimV '++))))

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
                 (match boo
                   [(BoolV bool) (cond [(equal? bool #t) (interp t env)]
                                       [else (interp e env)])]
                   [other (error 'interp "AAQZ : Given non-boolean type")])]
                             
    [(LamC p b) (CloV p b env)]
    [(AppC f a) (define f-value (interp f env))
                (define val-lst (map (lambda ([arg : ExprC]) (interp arg env)) a))
                (match f-value
                  [(CloV p b e)
                   (cond
                     [(equal? (length p) (length val-lst)) (interp b (extend-helper p val-lst e))]
                     [else (error 'interp "AAQZ : Wrong number of args")])]
                  [(PrimV sym) (interp-PrimV sym val-lst)]
                  [other (error 'interp "AAQZ : closure or primitive in function application")])]))

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
(define (interp-PrimV [sym : Symbol] [val-lst : (Listof Value)]) : Value
  (match* (sym val-lst)
    [('+ (list (NumV l) (NumV r))) (NumV (+ l r))]
    [('- (list (NumV l) (NumV r))) (NumV (- l r))]
    [('* (list (NumV l) (NumV r))) (NumV (* l r))]
    [('/ (list (NumV l) (NumV r))) (cond
                                     [(zero? r) (error 'interp-PrimV "AAQZ : Cannot divide by 0")]
                                     [else (NumV (/ l r))])]
    [('<= (list (NumV l) (NumV r))) (BoolV (<= l r))]
    ; Functions like eq?
    ; Neither l or r is a CloV or PrimV and 2 values are equal
    [('equal? (list l r))
     (cond [(and (and (not (CloV? l)) (not (PrimV? l))) (and (not (CloV? r)) (not (PrimV? r))))
            (BoolV (equal? l r))]
           [else (BoolV #f)])]
    [('println (list (StrV s))) (printf "~a" s) (newline) (BoolV (string? s))]
    [('read-num '()) (printf ">") (define input (string->number (cast (read-line) String)))
                     (cond
                       [(real? input) (NumV input)]
                       [else (error 'interp "AAQZ : Input not a number")])]
    [('read-str '()) (printf ">") (StrV (cast (read-line) String))]
    [('seq (? list? lst-val)) (seq-helper lst-val)]
    [('++ (? list? s)) (define new-str (++helper s)) (StrV new-str)]
    [('error (list val)) (error 'interp "AAQZ : user-error ~s" (serialize val))]
    [(_ _) (error 'interp-PrimV "AAQZ : Invalid PrimV")]))

; goes through the list of expressions and returns the last one.
(define (seq-helper [lst-val : (Listof Value)]) : Value
  (match lst-val
    [(cons f '()) f]
    [(cons f r) (seq-helper r)]))

; appends all strings in a list into 1 string in order
(define (++helper [lst-str : (Listof Value)]) : String
  (match lst-str
    [(cons f r)
     (match f
       [(StrV s) (string-append s (++helper r))]
       [(NumV n) (string-append (number->string n) (++helper r))]
       [other (error '++ "AAQZ : Invalid type")])]
    ['() ""]))
; Combines parsing and evaluation
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

(define example-program 
  '(seq
    {println "Input any random number from 0-5000"}
    {bind
     [seed = {read-num}]
     ; generates a random seed given a value
     [randomize-seed = {{seed} =>
                               ; gets the modulo given a numerator and denominator
                               (bind [mod = {{self num den} =>
                                                            {if {<= num den}
                                                                {if {equal? num den} 0 num}
                                                                {self self {- num den} den}}}]
                                     (mod mod {+ {* 32 seed} 43} 4294967296))}]
     (bind [new-seed = (randomize-seed seed)]
           {bind
            [empty = 15]
            {bind
             [empty? = {(x) => {equal? x empty}}]
             [cons = {(f r)
                      =>
                      {(key)
                       =>
                       {if {equal? key 0}
                           f
                           r}}}]
             [first = {(pair) => {pair 0}}]
             [rest =  {(pair) => {pair 1}}]
             (bind
              [deck-vals
               =
               (cons "10"
                     (cons "4"
                           (cons "3"
                                 (cons "A"
                                       (cons "6"
                                             (cons "J"
                                                   (cons "8"
                                                         (cons "5"
                                                               (cons "K"
                                                                     (cons "7"
                                                                           (cons "2"
                                                                                 (cons "Q"
                                                                                       (cons "9" empty)))))))))))))]
              [deck-suits = (cons "H"
                                  (cons "D"
                                        (cons "C"
                                              (cons "S" empty))))]
              [print-lst-lst = {(self l)
                                =>
                                {if {empty? l}
                                    empty
                                    {bind [suit = (first (first l))]
                                          [val = (first (rest (first l)))]
                                          {seq {println {++ suit val}}
                                               {self self {rest l}}}}}}]
                             
              [new-deck
               =
               {{self dvals dsuits}
                =>
                {if
                 {empty? dsuits}
                 empty
                 {bind
                  [add-suits
                   =
                   {{self suit vals} =>
                                     {if {empty? vals}
                                         empty
                                         {bind
                                          [elem = (first vals)]
                                          {cons {cons suit
                                                      (cons (first vals)
                                                            empty)}
                                                {self self suit (rest vals)}}}}}]
                  [link-decks
                   =
                   {{self deck1 deck2} =>
                                       {if
                                        {empty? deck1}
                                        deck2
                                        {cons {first deck1}
                                              {self self {rest deck1} deck2}}}}]
                                                               
                  {link-decks link-decks
                              {add-suits add-suits
                                         (first dsuits)
                                         dvals}
                              {self self dvals {rest dsuits}}}}}}]

              ; fisher-yates' shuffle 
              [shuffle
               =
               {{self deck len rng}
                => {if
                    {equal? len 1}
                    deck
                    {bind
                     [get-int =
                              {{max}
                               =>
                               (bind [mod
                                      =
                                      {{self num den} =>
                                                      {if {<= num den}
                                                          {if {equal? num den} 0 num}
                                                          {self self {- num den} den}}}]
                                     (mod mod rng max))}]
                     {bind [rand-idx = {get-int len}]
                           [node-idx =
                                     {{self cur idx lst}
                                      =>
                                      {if
                                       {equal? cur idx}
                                       lst
                                       {self self {+ cur 1} idx {rest lst}}}}]
                           [first-half
                            =
                            {{self cur idx lst}
                             =>
                             {if
                              {equal? cur idx}
                              empty
                              {cons {first lst} {self self {+ cur 1} idx {rest lst}}}}}]
                           [swap
                            =
                            {{self f-lst s-lst}
                             =>
                             {if
                              {empty? s-lst}
                              f-lst
                              {cons {first s-lst} {self self f-lst {rest s-lst}}}}}]
                           {seq
                            #; {println {++ "rand-idx: " rand-idx}}
                            (self self
                                  {swap swap
                                        {first-half first-half 0 rand-idx deck}
                                        {node-idx node-idx 0 rand-idx deck}}
                                  {- len 1}
                                  rng)}}}}}]
              [shuffle-n-times = {{self n f fself deck len rng}
                                  =>
                                  {if
                                   {<= n 1}
                                   {f fself deck len rng}
                                   {f fself {self self {- n 1} f fself deck len rng} len rng}}}]
                  
              {seq
               {println {++ "Generated seed: " new-seed}}
               #; {print-lst-lst print-lst-lst
                                 {new-deck new-deck deck-vals deck-suits}}
               #; {print-lst-lst print-lst-lst
                                 {shuffle-n-times shuffle-n-times 1
                                                  shuffle shuffle {new-deck new-deck deck-vals deck-suits} 52 new-seed}}
               {println "Welcome to BlackJack!"}
               {println "How to Play:"}
               {println "0. Both dealer and player begins with 2 cards"}
               {println "1. Beat the dealer by getting a higher value hand"}
               {println "2. Don't get a value higher than 21"}
               {println "3. Ace cards have a value of either 1 or 11"}
               {println "4. All Face cards have a value of 10"}
               {println "Good Luck!"}
               (bind
                [blackjack
                 = {{self
                     deck
                     player
                     dealer
                     cards
                     turn
                     rng} =>
                          {if
                           {<= cards 48}

                           ; Player begins playing
                           {if {equal? turn 0}
                               {bind
                                ; example: hand = '(("D" "3") ("D" "1"))
                                [reveal-hand
                                 =
                                 {{self hand} =>
                                              {if
                                               {empty? hand}
                                               ""
                                               {bind
                                                [hand-suit = (first (first hand))]
                                                [hand-val = (first (rest (first hand)))]
                                                {++ hand-suit hand-val " " {self self (rest hand)}}}}}]
                                {seq
                                 {println {++ "Your hand: " {reveal-hand reveal-hand player}}}
                                 {bind
                                  [calculate
                                   =
                                   {{self
                                     hand
                                     val
                                     aces} =>
                                           {if
                                            {empty? hand}
                                            {if
                                             {<= val 21}
                                             val
                                             {if
                                              {equal? aces 0}
                                              val
                                              {self self empty {- val 10}
                                                    {- aces 1}}}}
                                            {bind [hand-val = (first (rest (first hand)))]
                                                  {if
                                                   {equal? hand-val "A"}
                                                   {self self {rest hand} {+ val 11} {+ aces 1}}
                                                   {if
                                                    {equal? hand-val "2"}
                                                    {self self {rest hand} {+ val 2} aces}
                                                    {if
                                                     {equal? hand-val "3"}
                                                     {self self {rest hand} {+ val 3} aces}
                                                     {if
                                                      {equal? hand-val "4"}
                                                      {self self {rest hand} {+ val 4} aces}
                                                      {if
                                                       {equal? hand-val "5"}
                                                       {self self {rest hand} {+ val 5} aces}
                                                       {if
                                                        {equal? hand-val "6"}
                                                        {self self {rest hand} {+ val 6} aces}
                                                        {if
                                                         {equal? hand-val "7"}
                                                         {self self {rest hand} {+ val 7} aces}
                                                         {if
                                                          {equal? hand-val "8"}
                                                          {self self {rest hand} {+ val 8} aces}
                                                          {if
                                                           {equal? hand-val "9"}
                                                           {self self
                                                                 {rest hand}
                                                                 {+ val 9}
                                                                 aces}
                                                           {self self
                                                                 {rest hand}
                                                                 {+ val 10}
                                                                 aces}}}}}}}}}}}}}]
                                  {bind [player-score = {calculate calculate player 0 0}]
                                        ; continue playing
                                        {seq
                                         {println {++ "Your score: " player-score}}
                                         {if
                                          {<= player-score 21}
                                          {seq 
                                           {println {++ "Do you want to hit (h) or stand (s)?"}}
                                           {bind
                                            [action = {read-str}]
                                            {if
                                             {equal? action "h"}
                                             ; draw a card
                                             {bind
                                              [deal-card
                                               =
                                               {{self
                                                 person
                                                 deck
                                                 cards}
                                                =>
                                                {cons
                                                 {bind
                                                  [get-int
                                                   =
                                                   {{max rng}
                                                    =>
                                                    (bind
                                                     [mod
                                                      =
                                                      {{self num den} =>
                                                                      {if {<= num den}
                                                                          {if {equal? num den} 0 num}
                                                                          {self self {- num den} den}}}]
                                                     (mod mod rng max))}]
                                                  [node-idx
                                                   =
                                                   {{self cur idx lst}
                                                    =>
                                                    {if
                                                     {equal? cur idx}
                                                     lst
                                                     {self self {+ cur 1} idx {rest lst}}}}]
                                                  {first {node-idx node-idx 0 {get-int cards rng} deck}}}
                                                      person}}]
                                              {self self deck
                                                    {deal-card deal-card
                                                               player
                                                               deck
                                                               {- cards 1}}
                                                    dealer
                                                    {- cards 1}
                                                    0
                                                    rng}}
                                             ; Stay, keep score, Dealer plays
                                             {self self deck
                                                   player
                                                   dealer
                                                   {- cards 1}
                                                   1
                                                   rng}}}}
                                          {println
                                           "You bust. Dealer wins. Better luck next time!"}}}}}}}

                                                          
                               ; Dealer draws cards
                               {bind
                                ; example: hand = '(("D" "3") ("D" "1"))
                                [reveal-hand
                                 =
                                 {{self
                                   hand} =>
                                         {if
                                          {empty? hand}
                                          ""
                                          {bind
                                           [hand-suit = (first (first hand))]
                                           [hand-val = (first (rest (first hand)))]
                                           {++ hand-suit hand-val " " {self self (rest hand)}}}}}]
                                {seq
                                 {println {++ "Dealer's hand: " {reveal-hand reveal-hand dealer}}}
                                 {bind
                                  [calculate
                                   =
                                   {{self
                                     hand
                                     val
                                     aces}
                                    =>
                                    {if
                                     {empty? hand}
                                     {if
                                      {<= val 21}
                                      val
                                      {if
                                       {equal? aces 0}
                                       val
                                       {self self empty {- val 10}
                                             {- aces 1}}}}
                                     {bind
                                      [hand-val
                                       =
                                       (first
                                        (rest (first hand)))]
                                      {if
                                       {equal? hand-val "A"}
                                       {self self {rest hand} {+ val 11} {+ aces 1}}
                                       {if
                                        {equal? hand-val "2"}
                                        {self self {rest hand} {+ val 2} aces}
                                        {if
                                         {equal? hand-val "3"}
                                         {self self {rest hand} {+ val 3} aces}
                                         {if
                                          {equal? hand-val "4"}
                                          {self self {rest hand} {+ val 4} aces}
                                          {if
                                           {equal? hand-val "5"}
                                           {self self {rest hand} {+ val 5} aces}
                                           {if
                                            {equal? hand-val "6"}
                                            {self self {rest hand} {+ val 6} aces}
                                            {if
                                             {equal? hand-val "7"}
                                             {self self {rest hand} {+ val 7} aces}
                                             {if
                                              {equal? hand-val "8"}
                                              {self self {rest hand} {+ val 8} aces}
                                              {if
                                               {equal? hand-val "9"}
                                               {self self
                                                     {rest hand}
                                                     {+ val 9}
                                                     aces}
                                               {self self
                                                     {rest hand}
                                                     {+ val 10}
                                                     aces}}}}}}}}}}}}}]
                                                              
                                  {bind
                                   [dealer-score = {calculate calculate dealer 0 0}]
                                   ; continue playing
                                   {seq {println {++ "Dealer's score: " dealer-score}}
                                        {if {<= dealer-score 21}
                                            {if {<= dealer-score 17}
                                                {seq 
                                                 {println "The Dealer hits!"}
                                                 ; draw a card
                                                 {bind
                                                  [deal-card
                                                   =
                                                   {{self
                                                     person
                                                     deck
                                                     cards}
                                                    =>
                                                    {cons
                                                     {bind
                                                      [get-int
                                                       =
                                                       {{max rng}
                                                        =>
                                                        (bind
                                                         [mod
                                                          =
                                                          {{self
                                                            num
                                                            den}
                                                           =>
                                                           {if {<= num den}
                                                               {if {equal? num den} 0 num}
                                                               {self self {- num den} den}}}]
                                                         (mod mod rng max))}]
                                                      [node-idx
                                                       =
                                                       {{self cur idx lst}
                                                        =>
                                                        {if
                                                         {equal? cur idx}
                                                         lst
                                                         {self self {+ cur 1} idx {rest lst}}}}]
                                                      {first {node-idx node-idx 0 {get-int cards rng} deck}}}
                                                     person}}]
                                                  {self self deck
                                                        player
                                                        {deal-card deal-card dealer deck cards}
                                                        {- cards 1}
                                                        1
                                                        rng}}}
                                                {bind
                                                 [pscore = {calculate calculate player 0 0}]
                                                 {if
                                                  {equal? pscore dealer-score}
                                                  {println "It's a tie, but technically you lose :/"}
                                                  {if
                                                   {equal? pscore 21}
                                                   {println "Blackjack! You Win!!! :D"}
                                                   {if
                                                    {<= pscore dealer-score}
                                                    {println "Dealer wins. Better luck next time!"}
                                                    {println "You Win!!! :D"}}}}}}
                                            {println "Dealer bust! You Win!!! :D"}}}}}}}}
                                                        

                           ; Give Player and Dealer a hand
                           {bind
                            [deal-card
                             =
                             {{self
                               person
                               deck
                               cards}
                              =>
                              {cons {bind
                                     [get-int
                                      =
                                      {{max rng}
                                       =>
                                       (bind
                                        [mod
                                         =
                                         {{self num den} =>
                                                         {if {<= num den}
                                                             {if {equal? num den} 0 num}
                                                             {self self {- num den} den}}}]
                                        (mod mod rng max))}]
                                     [node-idx
                                      =
                                      {{self cur idx lst}
                                       =>
                                       {if
                                        {equal? cur idx}
                                        lst
                                        {self self {+ cur 1} idx {rest lst}}}}]
                                     {first {node-idx node-idx 0 {get-int cards rng} deck}}}
                                    person}}]
                            {if
                             {equal? turn 0}

                             ; Player gets a card
                             {self self deck
                                   {deal-card deal-card player deck cards}
                                   dealer
                                   {- cards 1}
                                   1
                                   rng}

                             ; Dealer gets a card
                             {self self deck
                                   player
                                   {deal-card deal-card dealer deck cards}
                                   {- cards 1}
                                   0
                                   rng}}}}}]
                (blackjack blackjack
                           {shuffle shuffle
                                    {new-deck new-deck deck-vals deck-suits}
                                    52 new-seed}
                           empty
                           empty
                           52 
                           0
                           new-seed))})}})}))

#; (top-interp example-program)

; ----------------------------------------------------------------------------------
; ----------------------------------- TEST CASES -----------------------------------
; ----------------------------------------------------------------------------------

; interp
(check-equal? (interp (IdC 'error) top-env) (PrimV 'error))
(check-equal? (interp (IdC 'println) top-env) (PrimV 'println))
(check-equal? (interp (IdC 'seq) top-env) (PrimV 'seq))
(check-equal? (interp (IdC 'read-num) top-env) (PrimV 'read-num))
(check-equal? (interp (IdC 'read-str) top-env) (PrimV 'read-str))


; top-interp
#; (check-exn (regexp (regexp-quote "AAQZ : Input not a number"))
              (lambda () (top-interp '{read-num})))
(check-equal? (top-interp '(bind [mod = {{self num den} => {if {<= num den}
                                                               {if {equal? num den} 0 num}
                                                               {self self {- num den} den}}}]
                                 (mod mod 19 3))) "1")
(check-equal? (top-interp '(bind [mod = {{self num den} => {if {<= num den}
                                                               {if {equal? num den} 0 num}
                                                               {self self {- num den} den}}}]
                                 (mod mod 19 19))) "0")
(check-equal? (top-interp '(bind [mod = {{self num den} => {if {<= num den}
                                                               {if {equal? num den} 0 num}
                                                               {self self {- num den} den}}}]
                                 (mod mod 5 2))) "1")
(check-equal? (top-interp '(bind [mod = {{self num den} => {if {<= num den}
                                                               {if {equal? num den} 0 num}
                                                               {self self {- num den} den}}}]
                                 (mod mod 21 6))) "3")

(check-equal? (top-interp '{bind
                            [get-int =
                                     {{min max}
                                      =>
                                      (bind [mod = {{self num den} => {if {<= num den}
                                                                          {if {equal? num den} 0 num}
                                                                          {self self {- num den} den}}}]
                                            (mod mod 267 {+ {- max min} 1}))}]
                            (get-int 0 51)}) "7")
(check-equal? (top-interp '{bind
                            [get-int =
                                     {{min max}
                                      =>
                                      (bind [mod = {{self num den} => {if {<= num den}
                                                                          {if {equal? num den} 0 num}
                                                                          {self self {- num den} den}}}]
                                            (mod mod 139 {+ {- max min} 1}))}]
                            (get-int 0 51)}) "35")
(check-equal? (top-interp '{bind
                            [get-int =
                                     {{min max}
                                      =>
                                      (bind [mod = {{self num den} => {if {<= num den}
                                                                          {if {equal? num den} 0 num}
                                                                          {self self {- num den} den}}}]
                                            (mod mod 107 {+ {- max min} 1}))}]
                            (get-int 0 51)}) "3")

#; (top-interp '{bind
                 [empty = 15]
                 {bind
                  [empty? = {(x) => {equal? x empty}}]
                  [cons = {(f r)
                           =>
                           {(key)
                            =>
                            {if {equal? key 0}
                                f
                                r}}}]
                  [first = {(pair) => {pair 0}}]
                  [rest =  {(pair) => {pair 1}}]
                  #; (first (cons 1 empty))
                  (first (rest (first (cons (cons 1 (cons 2 empty)) empty))))
                  #; {bind
                      [print-lst = {(self l)
                                    =>
                                    {if {empty? l}
                                        empty
                                        {bind [elem = (first l)]
                                              {seq {println elem}
                                                   {self self {rest l}}}}}}]
                      [my-list = {cons "3"
                                       {cons "24"
                                             {cons "8" empty}}}]
                      {println {++ "Printing my-list: " {print-lst print-lst my-list}}}}}})

