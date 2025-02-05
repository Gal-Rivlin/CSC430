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
(struct PrimV ([id : Symbol] [left : ExprC] [right : ExprC]))


;; parse, takes in an Sexp and returns an exprc

(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? n) (NumC n)]
    [(? string? s) (StrC s)]
    [(? symbol? l) (if (valid-sym l) (IdC l)
                       (error 'parse "QWJZ - invalid identifier name"))] ; symbols can't be proc/if/in
    [(list 'if b t e) (IfC (parse b) (parse t) (parse e))]
    [(list 'proc (list (? symbol? id) ...) body)
     (LamC (map (lambda ([sym : Symbol]) (IdC sym)) (cast id (Listof Symbol))) (parse body))]
    [(list 'declare (list pairs ...) 'in body)
     (AppC (LamC (cast (map (lambda ([x : Sexp]) (asgn-pairs x #t)) pairs) (Listof IdC)) (parse body))
           (map (lambda ([x : Sexp]) (asgn-pairs x #f)) pairs))]
    [(list func args ...) (AppC (parse func) (map parse args))]))

;check if the symbol we used is a valid identifier
(define (valid-sym [sym : Sexp]) : Boolean
  (and (symbol? sym) (not (or (equal? sym 'if) (equal? sym 'proc) (equal? sym 'in)))))

;take in (hopefully) a pair of identifier - value and return a chosen value/identifier
(define (asgn-pairs [pair : Sexp] [ord : Boolean]) : ExprC
  (match pair
    [(list (? symbol? a) b) (if ord (IdC a) (parse b))]
    [other (error 'asgn-pairs "QWJZ - Syntax Error: declaration must be id - val format")]))


;; interp : takes in an expression and its enviroment returns a value

(define (interp [e : ExprC] [envir : (Listof Bind)]) : ValV
  ;needs to handle: numc , strc , idc , ifc , appc , lamc
  (match e
    [(NumC n) (NumV n)]
    [(IdC id) (find-bind (cast id Symbol) envir)]
    [(StrC s) (StrV s)]
    [(IfC b t else) (if (equal? (interp b envir) (BoolV #t))
                        (interp t envir)
                        (interp else envir))]
    [(LamC params body) (CloV (map (lambda ([sym : IdC]) (IdC-id sym)) params) body envir)]
    [(AppC fun args) (interp-appc fun args envir)]))

;; find-bind, search the environment for the variable and return its value
(define (find-bind [sym : Symbol] [env : (Listof Bind)]) : ValV
  (match (filter (lambda ([b : Bind]) (symbol=? (Bind-id b) sym)) env)
    [(list (Bind _ val)) val]
    [_ (error 'find-bind "QWJZ - Runtime error: unbound identifier ~e" sym)]))

;; interp-appc, helper function that interps the AppC
(define (interp-appc [fun : ExprC] [args : (Listof ExprC)] [envir : (Listof Bind)]) : ValV
  (define f-value (interp fun envir)) ;1. interp the function
  (unless (CloV? f-value) ; Check if f-value is a CloV, if not error
    (error 'interp-appc "QWJZ - Runtime error: expected a CloV, but got" f-value))
  (define args-values (map (lambda ([arg : ExprC]) (interp arg envir)) args)) ;2. interp the args
  (interp (CloV-body f-value) ;4. interp the body in new extended environment
          (append (map Bind (CloV-params f-value) ;3. extend environment
                       args-values) 
                  (CloV-env f-value))))


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

;from textbook
#;(check-exn (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                             (numC 4)))
                           (numC 3))
                     mt-env)
             "name not found")









