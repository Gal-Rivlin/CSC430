#lang typed/racket
 
(require typed/rackunit)

;; Assignment 4: Bryn Harper and Gal Rivlin
;; Full project implemented


;; ExprC Data Definition (for parsing)
(define-type ExprC (U NumC StrC IdC AppC IfC LamC))
(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IdC ([id : Symbol])#:transparent)
(struct IfC ([bool : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct AppC ([func : ExprC] [args : (Listof ExprC)])#:transparent)
(struct LamC ([params : (Listof IdC)] [body : ExprC])#:transparent)


;; Val data defintitions (for interp)

(define-type ValV (U NumV BoolV StrV CloV PrimV))
(struct NumV ([n : Real]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : (Listof Bind)]))
(struct Bind ([id : Symbol] [val : ValV]) #:transparent)
(struct PrimV ([id : Symbol]))


;; parse, takes in an Sexp and returns an exprc

(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? n) (NumC n)]
    [(? string? s) (StrC s)]
    [(? symbol? l) (if (valid-sym l) (IdC l)
                       (error 'parse "QWJZ - invalid identifier name"))] ; symbols can't be proc/if/in
    [(list 'if b t e) (IfC (parse b) (parse t) (parse e))]
    [(list 'proc (list (? symbol? id) ...) body)
     (if (check-duplicates (cast id (Listof Symbol)))
         (error 'parse "QWJZ - Syntax Error: Duplicate argument names in function call ~e" id)
         (LamC (map (lambda ([sym : Symbol]) (IdC sym)) (cast id (Listof Symbol))) (parse body)))]
    [(list 'declare (list pairs ...) 'in body)
     (if (check-duplicates (get-vars pairs))
         (error 'parse "QWJZ - Syntax Error: Duplicate argument names in function call")
         (AppC (LamC (cast (map (lambda ([x : Sexp]) (asgn-pairs x #t)) pairs) (Listof IdC)) (parse body))
               (map (lambda ([x : Sexp]) (asgn-pairs x #f)) pairs)))]
    [(list func args ...) (AppC (parse func) (map parse args))]))

;; get the list of variables from a list of pairs
(define (get-vars [pairs : (Listof Sexp)]) : (Listof Sexp)
  (match pairs
    ['() '()]
    [(cons (list (? symbol? var) _) r) (if (valid-sym var)
                                           (cons var (get-vars r))
                                           (error 'get-vars "QWJZ - Syntax Error: invalid identifier ~e" var))]
    [other (error 'get-vars "QWJZ - Syntax Error: declaration must be id - val format")]))

;check if the symbol we used is a valid identifier
(define (valid-sym [sym : Sexp]) : Boolean
  (and (symbol? sym) (not (or (equal? sym 'if) (equal? sym 'proc) (equal? sym 'in) (equal? sym 'declare)))))

;take in (hopefully) a pair of identifier - value and return a chosen value/identifier
(define (asgn-pairs [pair : Sexp] [ord : Boolean]) : ExprC
  (match pair
    [(list (? symbol? a) b) (if ord (IdC a) (parse b))]))


;; interp : takes in an expression and its enviroment returns a value

(define (interp [e : ExprC] [envir : (Listof Bind)]) : ValV
  ;needs to handle: numc , strc , idc , ifc , appc , lamc
  (match e
    [(NumC n) (NumV n)]
    [(IdC id) (find-bind id envir)]
    [(StrC s) (StrV s)]
    [(IfC b t else) (define boolstate (interp b envir))
                    (if (BoolV? boolstate)
                        (if (my-equal? boolstate (BoolV #t))
                            (interp t envir)
                            (interp else envir))
                        (error 'interp "QWJZ - conditional is not a boolean ~e" boolstate))] 
    [(LamC params body) (CloV (map (lambda ([sym : IdC]) (IdC-id sym)) params) body envir)]
    [(AppC fun args) (interp-appc fun args envir)]))


;; find-bind, search the environment for the variable and return its value
;;if variable is not found but is a prim operator, cast it into a prim
(define (find-bind [sym : Symbol] [env : (Listof Bind)]) : ValV
  (match (filter (lambda ([b : Bind]) (symbol=? (Bind-id b) sym)) env)
    [(list (Bind _ val)) val]
    [_ (match sym
         [(? prim-op? s) (PrimV s)]
         ['true (BoolV #t)]
         ['false (BoolV #f)]
         [other (error 'find-bind "QWJZ - Runtime error: unbound identifier ~e" sym)])]))

; prim-op? checks if the function call is a primitive operator
(define (prim-op? [e : Symbol]) : Boolean
  (or (equal? e '+) (equal? e '-) (equal? e '/) (equal? e '*)
      (equal? e '<=) (equal? e 'equal?) (equal? e 'error)))

;; interp-appc, helper function that interps the AppC
(define (interp-appc [fun : ExprC] [args : (Listof ExprC)] [envir : (Listof Bind)]) : ValV
  (define f-value (interp fun envir)) ;1. interp the function
  (define args-values (map (lambda ([arg : ExprC]) (interp arg envir)) args)) ;2. interp the args
  (match f-value
    [(CloV par body env)
     (if (= (length par) (length args-values))
         (interp body ;4. interp the body in new extended environment
                 (append (map Bind par args-values) env))
         (error 'interp-appc "QWJZ - wrong number of arguments pushed"))
     ] ;3. extend environment
    [(PrimV id) (prim-interp id args-values)]
    [other (error 'interp-appc "QWJZ - Runtime error: expected a CloV, but got ~e" f-value)]))

;takes in a primitive operator, the arguments, and its enviroment. returns a value
(define (prim-interp [type : Symbol] [args-values : (Listof ValV)]) : ValV
  (match* (type args-values)
    [('+ (list (NumV left) (NumV right))) (NumV (+ left right))]
    [('- (list (NumV left) (NumV right))) (NumV (- left right))]
    [('/ (list (NumV left) (NumV right)))
     (if (= right 0)
         (error 'prim-interp "QWJZ - Cannot divide by zero")
         (NumV (/ left right)))]
    [('* (list (NumV left) (NumV right))) (NumV (* left right))]
    [('<= (list (NumV left) (NumV right))) (BoolV (<= left right))]
    [('equal? (list left right)) (BoolV (my-equal? left right))] ; fix this
    [('error (list message)) (error 'prim-interp "QWJZ - user-error")] ; fix this
    [(other-op other-args) (error 'prim-interp "QWJZ - wrong formatting for prim-operator")]))

;; Returns true if neither value is a closure or a primitive operator and the two values are equal
(define (my-equal? [val1 : ValV] [val2 : ValV]) : Boolean
  (cond
    [(or (CloV? val1) (CloV? val2)) #f]  ;check types first
    [(or (PrimV? val1) (PrimV? val2)) #f]
    [else
     (match* (val1 val2)
       [((NumV n1) (NumV n2)) (if (= n1 n2) #t #f)]
       [((StrV s1) (StrV s2)) (if (string=? s1 s2) #t #f)]
       [((BoolV b1) (BoolV b2)) (if (or (and b1 b2) (and (not b1) (not b2))) #t #f)]
       [(_ _)#f])]))  ; If types don’t match, return #f

;; serialize - accepts a valv and returns it as a string

(define (serialize [val : ValV]) : String
  (match val
    [(NumV n) (format "~v" n)]
    [(StrV s) (format "~v" s)]
    [(BoolV #t) "true"]
    [(BoolV #f) "false"]
    [(CloV p b e) "#<procedure>"]
    [(PrimV i) "#<primop>"]))


;; top-interp - accepts an QWJZ4 syntax and returns the value as a string

(define (top-interp [exp : Sexp]) : String
  (serialize (interp (parse exp) '())))

;; extend-env extends the closure's environment with new bindings
#;(define (extend-env [cloEnv : (Listof Bind)] [curEnv : (Listof Bind)]) : (Listof Bind)
    )

;; Copied from textbook, will help later when dealing with operators
#;(define (num+ [l : Value] [r : Value]) : Value
    (cond
      [(and (NumV? l) (NumV? r))
       (NumV (+ (NumV-n l) (NumV-n r)))]
      [else
       (error 'num+ "one argument was not a number")]))


;; Test Cases


;parse tests

(check-equal? (parse 5) (NumC 5))
(check-equal? (parse 'hi) (IdC 'hi))
(check-equal? (parse "gal") (StrC "gal"))
(check-equal? (parse '{if "true" 5 hello}) (IfC (StrC "true") (NumC 5) (IdC 'hello)))
(check-equal? (parse '{{proc {x y} {+ x y}} 5 8})
              (AppC (LamC (list (IdC 'x) (IdC 'y)) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 5) (NumC 8))))

(check-equal? (parse '{declare {[x 5] [y 3]} in {+ x y}})
              (AppC (LamC (list (IdC 'x) (IdC 'y)) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 5) (NumC 3))))


; parse test errors

(check-exn #rx"QWJZ - invalid identifier name"
           (lambda () (parse 'in)))

(check-exn #rx"QWJZ - Syntax Error: declaration must be id - val format"
           (lambda () (parse '{declare {[x 5] [3]} in {+ x y}})))

(check-exn #rx"QWJZ - Syntax Error: declaration must be id - val format"
           (lambda () (parse '{declare {[x 5] [5 3]} in {+ x y}})))

(check-exn #rx"QWJZ - Syntax Error: Duplicate argument names in function call"
           (lambda () (parse '(proc (x x) 3))))

(check-exn #rx"QWJZ - Syntax Error: Duplicate argument names in function call"
           (lambda () (parse '{declare {[x 5] [x 3]} in 3})))

(check-exn #rx"QWJZ - Syntax Error: "
           (lambda () (parse '(declare ((declare "")) in "World"))))


;; find-bind tests
(check-equal? (find-bind 'a (list (Bind 'b (NumV 1)) (Bind 'c (NumV 2)) (Bind 'a (NumV 3))))
              (NumV 3))
(check-exn #rx"QWJZ - Runtime error: unbound identifier 'x"
           (lambda () (find-bind 'x (list (Bind 'a (StrV "hi"))))))
(check-equal? (find-bind 'str (list (Bind 'str (StrV "This is the string"))))
              (StrV "This is the string"))

;; interp tests
(check-equal? (interp (NumC 2) '()) (NumV 2))
(check-equal? (interp (StrC "haha") '()) (StrV "haha"))
(check-equal? (interp (AppC (LamC (list (IdC 'x) (IdC 'y))
                                  (AppC (IdC '+)
                                        (list (IdC 'x) (IdC 'y))))
                            (list (NumC 5) (NumC 7)))
                      '())
              (NumV 12))

(check-equal? (interp (parse '{if {equal? 5 5} 10 20}) '()) (NumV 10))
(check-equal? (interp (parse '{if {equal? "str1" "str2"} 10 20}) '()) (NumV 20))
(check-equal? (interp (parse '{if {equal? "hi" "hi"} "yes" "no"}) '()) (StrV "yes"))
(check-exn #rx"QWJZ - Cannot divide by zero"
           (lambda () (top-interp '(/ 1 (- 3 3)))))

;interp-parse tests

(check-equal? (interp (parse '{+ 3 5}) '()) (NumV 8))
(check-equal? (interp (parse '{- 3 5}) '()) (NumV -2))
(check-equal? (interp (parse '{* 3 5}) '()) (NumV 15))
(check-equal? (interp (parse '{/ 10 5}) '()) (NumV 2))
(check-equal? (interp (parse '{{proc {+} {+ 2 3}} {proc {x y} {* x y}}}) '()) (NumV 6))
(check-equal? (interp
               (parse '{declare
                        {[pow {proc (base nat self)
                                    {if {<= nat 0}
                                        1
                                        {* base {self base {- nat 1} self}}}}]}
                        in {pow 3 4 pow}} ) '()) (NumV 81))
(check-equal? (interp (parse '{declare {[fact {proc (self n)
                                                    {if {<= n 0}
                                                        1
                                                        {* n {self self {- n 1}}}}}]}
                                       in {fact fact 6}} ) '()) (NumV 720))

(check-equal? (interp (parse '{if true {+ 3 2} 8}) '()) (NumV 5)) ;THIS SHOULDN'T BE FAILING
(check-equal? (interp (parse '{if false {+ 3 2} 8}) '()) (NumV 8))

;; my-equal? tests
(check-equal? (my-equal? (StrV "str1") (StrV "str1")) #t)
(check-equal? (my-equal? (StrV "str1") (StrV "str2")) #f)
(check-equal? (my-equal? (StrV "str") (NumV 1)) #f)
(check-equal? (my-equal? (NumV 1) (NumV 2)) #f)
(check-equal? (my-equal? (NumV 1) (NumV 1)) #t)
(check-equal? (my-equal? (BoolV #t) (BoolV #t)) #t)
(check-equal? (my-equal? (BoolV #t) (BoolV #f)) #f)
(check-equal? (my-equal? (CloV (list 'x) (NumC 1) '()) (StrV "str")) #f)
(check-equal? (my-equal? (PrimV '+)(PrimV '+)) #f)

;from textbook
#;(check-exn (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                             (numC 4)))
                           (numC 3))
                     mt-env)
             "name not found")


; top - interp tests

(check-equal? (top-interp '{declare {[fact {proc (self n)
                                                 {if {<= n 0}
                                                     1
                                                     {* n {self self {- n 1}}}}}]}
                                    in {fact fact 6}}) "720")

(check-equal? (top-interp '{if (<= 3 5) "hello" "goodbye"}) "\"hello\"")

(check-equal? (top-interp '{<= {+ 2 3} 8}) "true")

(check-equal? (top-interp '{<= {* 8 3} 8}) "false")

(check-equal? (top-interp '{declare {
                                     [func {proc (x) {proc {y} {+ x y}}}]}
                                    in {func 3}}) "#<procedure>")

(check-equal? (top-interp '{{declare {
                                      [func {proc (x) {proc {y} {* x y}}}]}
                                     in {func 3}} 6}) "18")
(check-equal? (top-interp '{if {<= {{declare {
                                              [func {proc (x) {proc {y} {* x y}}}]}
                                             in {func 3}} 6} 18} + false}) "#<primop>")




(check-exn #rx"QWJZ - user-error"
           (lambda () (top-interp '{error "hello"})))

(check-exn #rx"QWJZ - wrong formatting for prim-operator"
           (lambda () (top-interp '{+ 3 3 4})))


(check-exn #rx"QWJZ - conditional is not a boolean"
           (lambda () (top-interp '{if {+ 3 5} true false})))

(check-exn #rx"QWJZ - Runtime error: expected a CloV, but got"
           (lambda () (top-interp '{declare {[x 5] [y {proc {f} {* f f}}]} in {x 8}})))

(check-exn #rx"QWJZ - wrong number of arguments pushed"
           (lambda () (top-interp '{declare {[x 5] [y {proc {f} {* f f}}]} in {y}})))

(check-exn #rx"QWJZ - wrong number of arguments pushed"
           (lambda () (top-interp '{declare {[x 5] [y {proc {f} {* f f}}]} in {y 6 5}})))










