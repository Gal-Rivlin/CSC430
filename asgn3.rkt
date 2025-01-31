#lang typed/racket
 
(require typed/rackunit)

;; Assignment 3: Bryn Harper and Gal Rivlin
;; Full project implemented


;; ExprC Data Definition
(define-type ExprC (U NumC IfLeq0? BinOp Id Fcall))
(struct NumC ([n : Real]) #:transparent)
(struct IfLeq0? ([val : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct BinOp ([operator : Symbol] [l : ExprC] [r : ExprC])#:transparent)
(struct Id ([id : Symbol])#:transparent)
(struct Fcall ([name : Id] [args : (Listof ExprC)])#:transparent)


;; FundefC data definition
(struct FundefC ([name : Id] [vars : (Listof Id)] [body : ExprC])#:transparent)


;; Parses an s-expression into an ExprC.
(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? n) (NumC n)]
    [(list (? is-op? n) left right) (BinOp (cast n Symbol) (parse left) (parse right))]
    [(? symbol? s) (if (is-not-valid s)
                       (error 'parse "QWJZ - Syntax Error: symbol cannot be an operator")
                       (Id s))]
    [(cons 'ifleq0? r) (match r
                         [(list val then else) (IfLeq0? (parse val) (parse then) (parse else))]
                         [other (error
                                 'parse
                                 "QWJZ - Syntax Error: wrong number of arguments in Ifleq0?")])]
    [(list (? is-op? n) x ...)
     (error 'parse "QWJZ - Syntax Error: wrong num of arguments in binop")]
    [(list (? symbol? s) x ...) (Fcall (Id s) (map parse x))]
    [other (error 'parse "QWJZ - Syntax Error: expected valid expression, got ~e" other)]))


;; is-op? checks if an s-expression is an operator
(define (is-op? [exp : Sexp]) : Boolean
  (or (equal? '+ exp) (equal? '- exp) (equal? '/ exp) (equal? '* exp)))

(define (is-not-valid [exp : Sexp]) : Boolean
  (or (is-op? exp) (equal? 'proc exp) (equal? '= exp) (equal? 'ifleq0? exp)))


;; parse-fundef parses an s-expression into a funcion definition
(define (parse-fundef [fundef : Sexp]) : FundefC
  (match fundef
    [(list 'main '= (list 'proc (list (? symbol? id) ...) exp))
     (if (= (length id) 0) (FundefC (Id 'main) '() (parse exp))
         (error 'parse-fundef "QWJZ - Syntax Error: main should not have any arguments")) ]
    [(list (? symbol? name) '= (list 'proc (list (? symbol? id) ...) exp))
     (if (is-not-valid name)
         ((error 'parse-fundef "QWJZ - Syntax Error: function name cannot be an operator"))
         (if (check-duplicates (cast id (Listof Symbol)))
         (error 'parse-fundef "QWJZ - Syntax Error: Duplicate argument names in function call")
         (FundefC (Id name) (map (lambda ([x : Symbol]) : Id
                                   (Id x)) (cast id (Listof Symbol))) (parse exp))))]
    [other (error 'parse-fundef "QWJZ - Syntax Error: Wrong function definition format")]))


;; parse-prog takes in an s-expression (our program) and returns a list of function definitions
(define (parse-prog [prog : Sexp]) : (Listof FundefC)
  (define prog1 (parse-prog-helper prog))
  (if (check-duplicates (map (lambda ([func : FundefC]) : Id
                                   (FundefC-name func)) prog1 ))
      (error 'parse-prog "QWJZ - duplicate function name")
      prog1))

(define (parse-prog-helper [prog : Sexp]) : (Listof FundefC)
  (match prog
    [(cons a b) (cons (parse-fundef a) (parse-prog b))]
    ['() '()]))


;; interp accepts an expression and list of functions and returns a real value
(define (interp [a : ExprC] [funs : (Listof FundefC) ]) : Real
  (match a
    [(NumC n) n]
    [(? BinOp? b) (do-binop b funs)]
    [(IfLeq0? val then else) (cond [(<= (interp val funs) 0) (interp then funs)]
                                   [else (interp else funs)])]
    [(Fcall id args) (define func (function-lookup id funs))
                     (if (arg-verify args (FundefC-vars func)) 
                         (interp (subs
                                  (FundefC-body func)
                                  (FundefC-vars func)
                                  (map (lambda ([mybod : ExprC]) : Real
                                   (interp mybod funs)) args)) funs)
                         (error 'function-sub "QWJZ - wrong number of arguments pushed"))]))


;; subs accepts the body of a function, a list of variables ids, and a list of argument values
;; and substitutes the argument values into the variables until no variables remain
(define (subs [body : ExprC] [vars : (Listof Id)] [args : (Listof Real)]) : ExprC
  (match body
    [(NumC n) body]
    [(BinOp o l r) (BinOp o (subs l vars args) (subs r vars args))]
    [(IfLeq0? val then else)
     (IfLeq0? (subs val vars args) (subs then vars args) (subs else vars args))]
    [(Fcall id args2) (Fcall id (map
                                (lambda ([mybod : ExprC]) : ExprC
                                   (subs mybod vars args)) args2))]
    [(Id s) (NumC (arg-find (Id s) args vars))]))


;;arg-find finds the corresponding ExprC to a symbol
(define (arg-find [find : Id] [args : (Listof Real)] [vars : (Listof Id)]) : Real
  (match* (args vars)
    [('() '()) (error 'arg-find "QWJZ - unbound identifier") ]
    [((cons f1 r1) (cons f2 r2)) (if (equal? find f2) f1 (arg-find find r1 r2))]))


;; do-binop takes in a binop expression and evaluates it into a value
(define (do-binop [bin : BinOp] [funs : (Listof FundefC) ]) : Real
  (match bin
    [(BinOp '+ l r) (+ (interp l funs) (interp r funs))]
    [(BinOp '- l r) (- (interp l funs) (interp r funs))]
    [(BinOp '* l r) (* (interp l funs) (interp r funs))]
    [(BinOp '/ l r)  (let ([result (interp r funs)])
       (if (equal? result 0)
           (error 'do-binop "QWJZ - division by zero not allowed")
           (/ (interp l funs) result)))]))


;; function-lookup accepts a name and a list of functions and finds the matching function
(define (function-lookup [name : Id] [funs : (Listof FundefC)] ) : FundefC
  (match funs
    [(cons a b) (if (equal? (FundefC-name a) name) a (function-lookup name b))]
    [other (error 'function-lookup "QWJZ - function name ~e not found" (Id-id name))]))


;; arg-verify accepts a function's args and the function call
;; and checks that the right amount of args were pushed
(define (arg-verify [args : (Listof ExprC)] [vars : (Listof Id)]) : Boolean
  (= (length vars) (length args)))


;; interp-fns accepts a list of function definitions and evaluates them into a value
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (interp (FundefC-body (function-lookup (Id 'main) funs)) funs))


;; top-interp accepts an s-expression program and calls the parser and then the interp function.
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))


;; Test Cases

;; BinOp tests
(check-equal? (interp (parse '{+ 1 2}) '()) 3)
(check-equal? (interp (parse '2) '()) 2)
(check-equal? (interp (parse '{* {+ 1 2} 3}) '()) 9)
(check-equal? (interp (parse '{/ 4 2}) '()) 2)
(check-equal? (interp (parse '{- 5 2}) '()) 3)

;; parse syntax error tests
(check-exn #rx"QWJZ - Syntax Error: wrong num of arguments in binop"
           (lambda () (parse '{- 3 4 5})))

(check-exn #rx"QWJZ - Syntax Error: symbol cannot be an operator"
           (lambda () (parse '{- / 5})))

(check-exn #rx"QWJZ - Syntax Error: wrong number of arguments in Ifleq0?"
           (lambda () (parse '{ifleq0?})))

(check-exn #rx"QWJZ - Syntax Error: Duplicate argument names"
           (lambda () (parse-fundef '{foo = {proc {x x} {+ x x}}})))

(check-exn #rx"QWJZ - Syntax Error: Duplicate argument names"
           (lambda () (parse-fundef '{foo = {proc {x x} {+ x y}}})))

(check-exn #rx"QWJZ - Syntax Error: Wrong function definition format"
           (lambda () (parse-fundef '{hi})))

(check-exn #rx"QWJZ - Syntax Error: function name cannot be an operator"
           (lambda () (parse-fundef '{+ = {proc {x y} {+ x y}}})))

;; ifleq0? tests
(check-equal? (parse '{ifleq0? 1 1 {- 1 1}})
              (IfLeq0? (NumC 1) (NumC 1) (BinOp '- (NumC 1) (NumC 1))))
(check-equal? (interp (parse '{ifleq0? 1 1 {- 1 1}}) '()) 0)
(check-equal? (interp (parse '{ifleq0? -1 5 0}) '()) 5)
(check-equal? (top-interp '{{f = {proc (x) {ifleq0? x {- x 2} {+ x 2}}}}
                             {main = {proc () {f 1}}}})
              3)

;; test parsing of fcall
(check-equal? (parse '{foo 1 4 {+ 3 5}})
              (Fcall (Id 'foo) (list (NumC 1) (NumC 4) (BinOp '+ (NumC 3) (NumC 5)))))

;; parse-fundef tests
(check-equal? (parse-fundef '{foo = {proc {x y} {+ x y}}})
              (FundefC (Id 'foo) (list (Id 'x) (Id 'y)) (BinOp '+ (Id 'x) (Id 'y))))

;; full program tests
(check-equal? (top-interp '{{f = {proc (x y) {+ x y}}}
                             {main = {proc () {f 1 2}}}})
              3)

(check-equal? (top-interp '{{f = {proc () 5}}
                             {main = {proc () {+ {f} {f}}}}})
              10)

(check-equal? (top-interp '{{f = {proc (x y z) {* {+ x y} z} }}
                             {g = {proc (x z) {- z x}}}
                             {main = {proc () {+ {f 3 2 4} {g 5 2}}}}})
              17)

(check-exn #rx"QWJZ - wrong number of arguments pushed"
           (lambda () (top-interp '{{f = {proc (x y z) {* {+ x y} z} }}
                                                {g = {proc (x z) {- z {f x}}}}
                                                {main = {proc () {+ {g 3 2 4} 3}}}})))

(check-exn #rx"QWJZ - unbound identifier"
           (lambda () (top-interp '{{f = {proc (x y z) {* {+ x y} m} }}
                                                {g = {proc (x z) {- z {f x x x}}}}
                                                {main = {proc () {+ {g 3 4} 3}}}})))

(check-exn #rx"QWJZ - function name 'l not found"
           (lambda () (top-interp '{{f = {proc (x y z) {* {+ x y} z} }}
                                                {g = {proc (x z) {- z {l x x x}}}}
                                                {main = {proc () {+ {g 3 4} 3}}}})))

(check-exn #rx"QWJZ - Syntax Error: main should not have any arguments"
           (lambda () (top-interp '{{f = {proc (x y) {+ x y}}}
                                    {main = {proc (blah) {f 1 2}}}})))

(check-exn #rx"QWJZ - unbound identifier"
           (lambda () (top-interp '{{f = {proc (x y z) {* {+ x y} m} }}
                             {g = {proc (x z) {- z {f x x x}}}}
                             {main = {proc () {ifleq0? {+ {g {ifleq0? -3 4 -9} 2} 3} 5 13}}}})))

(check-exn #rx"QWJZ - duplicate function name"
           (lambda () (top-interp '{{g = {proc (x y z) {* {+ x y} m} }}
                             {g = {proc (x z) {- z {f x x x}}}}
                             {main = {proc () {ifleq0? {+ {g {ifleq0? -3 4 -9} 2} 3} 5 13}}}})))

(check-exn #rx"QWJZ - division by zero not allowed"
           (lambda () (top-interp '{
                             
                             {main = {proc () {/ 5 0}}}})))

(check-exn #rx"QWJZ - Syntax Error: expected valid expression, got"
           (lambda () (top-interp
               '{
                 
                 {main = {proc () {5}}}}) ))

(check-equal? (top-interp '{{f = {proc (x y z) {* {+ x y} z} }}
                             {g = {proc (x z) {- z {f x x x}}}}
                             {main = {proc () {+ {g 3 2} 3}}}})
              -13)

(check-equal? (top-interp '{{f = {proc (x y z) {* {+ x y} z} }}
                             {g = {proc (x z) {- z {f x x x}}}}
                             {main = {proc () {ifleq0? {+ {g 3 2} 3} 5 13}}}})
              5)

(check-equal? (top-interp '{{f = {proc (x y z) {* {+ x y} z} }}
                             {g = {proc (x z) {- z {f x x x}}}}
                             {main = {proc () {ifleq0? {+ {g {ifleq0? -3 4 -9} 2} 3} 5 13}}}})
              5)

(check-equal? (top-interp
               '{
                 {g = {proc (x) {ifleq0? x 1 {* x {g {- x 1}}}}}}
                 {main = {proc () {g 5}}}})
              120)





