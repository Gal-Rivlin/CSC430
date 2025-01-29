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
(struct FundefC ([name : Id] [args : (Listof Id)] [body : ExprC])#:transparent)


;; Parses an s-expression into an ExprC.
(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? n) (NumC n)]
    [(list (? is-op? n) left right) (BinOp (cast n Symbol) (parse left) (parse right))]
    [(? symbol? s) (Id s)]
    [(list 'ifleq0? val then else) (IfLeq0? (parse val) (parse then) (parse else))]
    [(list (? symbol? s) x ...) (Fcall (Id s) (map parse x))]
    [other (error 'parse "QWJZ - expected valid expression, got ~e" other)]))


;; is-op? checks if an s-expression is an operator
(define (is-op? [exp : Sexp]) : Boolean
  (or (equal? '+ exp) (equal? '- exp) (equal? '/ exp) (equal? '* exp)))


;; parse-fundef parses an s-expression into a funcion definition
(define (parse-fundef [fundef : Sexp]) : FundefC
  (match fundef
    [(list (? symbol? name) '= (list 'proc (list (? symbol? id) ...) exp))
     (if (check-duplicates (cast id (Listof Symbol)))
         (error 'parse-fundef "QWJZ - Syntax Error: Duplicate argument names in function call")
         (FundefC (Id name) (map (lambda ([x : Symbol]) : Id
                                   (Id x)) (cast id (Listof Symbol))) (parse exp)))]
    [other (error 'parse-fundef "QWJZ - Syntax Error: Wrong function definition format")]))


;; parse-prog take in an s-expression (our program) and returns a list of function definitions
(define (parse-prog [prog : Sexp]) : (Listof FundefC)
  (match prog
    [(cons a b) (cons (parse-fundef a) (parse-prog b))]
    ['() '()]))


;; interp
(define (interp [a : ExprC]) : Real
  (match a
    [(NumC n) n]
    [(? BinOp? b) (do-binop b)]
    [(IfLeq0? val then else) (cond [(<= (interp val) 0) (interp then)]
                                   [else (interp else)])]
    [other (error 'interp "QWJZ - unbound identifer")]))


;; do-binop : takes in a binop expression and returns a value
(define (do-binop [bin : BinOp]) : Real
  (match bin
    [(BinOp '+ l r) (+ (interp l) (interp r))]
    [(BinOp '- l r) (- (interp l) (interp r))]
    [(BinOp '* l r) (* (interp l) (interp r))]
    [(BinOp '/ l r) (/ (interp l) (interp r))]
    #;[other (error 'do-binop "QWJZ - unimplemented operand")]) )


;; interp-fns , takes a list of fundefc's and evaluates them
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (function-sub (function-lookup (Id 'main) funs) '() funs))


;; function-lookup , given a name and a list of functions, find the function
(define (function-lookup [name : Id] [funs : (Listof FundefC)] ) : FundefC
  (match funs
    [(cons a b) (if (equal? (FundefC-name a) name) a (function-lookup name b))]
    [other (error 'function-lookup "QWJZ - function name ~e not found" (Id-id name))]))


;; function-sub , given a function, its variables, and the list of functions in the program,
;; substitute the body of the function until a value is produced.
(define (function-sub [func : FundefC] [vars : (Listof Real)] [funs : (Listof FundefC)]) : Real
  (match (FundefC-body func)
    [(BinOp op l r)
     (interp (BinOp op
                    (NumC (function-sub
                           (FundefC (FundefC-name func) (FundefC-args func) l) vars funs))
                    (NumC (function-sub
                           (FundefC (FundefC-name func) (FundefC-args func) r) vars funs))))]
    [(NumC n) (interp (NumC n))]
    [(IfLeq0? v t e) (interp (IfLeq0?
                              (NumC (function-sub
                                     (FundefC
                                      (FundefC-name func) (FundefC-args func) v) vars funs))
                              (NumC (function-sub
                                     (FundefC
                                      (FundefC-name func) (FundefC-args func) t) vars funs))
                              (NumC (function-sub
                                     (FundefC
                                      (FundefC-name func) (FundefC-args func) e) vars funs))))]
    [(Fcall n args1) (if (arg-verify args1 (FundefC-args (function-lookup n funs))) 
                         (function-sub
                          (function-lookup n funs) (arg-simplify args1 func vars funs) funs)
                         (error 'function-sub "QWJZ - wrong number of arguments pushed"))]
    [(Id s) (arg-find (Id s) vars (FundefC-args func)) ]))


;; arg-simplify, given arguments and conditions of calling function,
;; evaluate them until they become reals
(define (arg-simplify [args : (Listof ExprC)]
                      [func : FundefC]
                      [vars : (Listof Real)]
                      [funs : (Listof FundefC)]) : (Listof Real)
  (match args
    [(cons a b) (cons (function-sub (FundefC (FundefC-name func) (FundefC-args func) a) vars funs)
                      (arg-simplify b func vars funs))]
    ['() '()]))


;; arg-verify, given function args and the function call,
;; check that the right amount of args were pushed
(define (arg-verify [vars : (Listof ExprC)] [args : (Listof Id)]) : Boolean
  (= (length vars) (length args)))


;;arg-find, find the corresponding exprc to a symbol
(define (arg-find [find : Id] [vars : (Listof Real)] [args : (Listof Id)]) : Real
  (match* (vars args)
    [('() '()) (error 'arg-find "QWJZ - unbound identifer") ]
    [((cons f1 r1) (cons f2 r2)) (if (equal? find f2) f1 (arg-find find r1 r2))]))


;; Accepts an s-expression and calls the parser and then the interp function.
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))


;; Test Cases

;; BinOp tests
(check-equal? (interp (parse '{+ 1 2})) 3) ;;should these be check-= tests instead??????
(check-equal? (interp (parse '2)) 2)
(check-equal? (interp (parse '{* {+ 1 2} 3})) 9)
(check-equal? (interp (parse '{/ 4 2})) 2)
(check-equal? (interp (parse '{- 5 2})) 3)

;; parse error tests
(check-exn #rx"QWJZ - expected valid expression"
           (lambda () (interp (parse "fail"))))
(check-exn #rx"QWJZ - unbound identifer"
           (lambda () (interp (parse '{bruh 5 2}))))
(check-exn #rx"QWJZ - unbound identifer"
           (lambda () (interp (parse '{fail}))))
(check-exn #rx"QWJZ - unbound identifer"
           (lambda () (interp (parse '{hi}))))

;; ifleq0? tests
(check-equal? (parse '{ifleq0? 1 1 {- 1 1}}) (IfLeq0? (NumC 1) (NumC 1) (BinOp '- (NumC 1) (NumC 1))))
(check-equal? (interp (parse '{ifleq0? 1 1 {- 1 1}})) 0) ; change this to variables
(check-equal? (interp (parse '{ifleq0? -1 5 0})) 5)

;; test parsing of fcall
(check-equal? (parse '{foo 1 4 {+ 3 5}}) (Fcall (Id 'foo) (list (NumC 1) (NumC 4) (BinOp '+ (NumC 3) (NumC 5)))))

;; parse-fundef tests
(check-equal? (parse-fundef '{foo = {proc {x y} {+ x y}}})
              (FundefC (Id 'foo) (list (Id 'x) (Id 'y)) (BinOp '+ (Id 'x) (Id 'y))))
(check-exn #rx"QWJZ - Syntax Error: Duplicate argument names"
           (lambda () (parse-fundef '{foo = {proc {x x} {+ x x}}})))
(check-exn #rx"QWJZ - Syntax Error: Duplicate argument names"
           (lambda () (parse-fundef '{foo = {proc {x x} {+ x y}}})))
(check-exn #rx"QWJZ - Syntax Error: Wrong function definition format"
           (lambda () (parse-fundef '{hi})))

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

(check-exn #rx"QWJZ - unbound identifer"
           (lambda () (top-interp '{{f = {proc (x y z) {* {+ x y} m} }}
                                                {g = {proc (x z) {- z {f x x x}}}}
                                                {main = {proc () {+ {g 3 4} 3}}}})))

(check-exn #rx"QWJZ - function name 'l not found"
           (lambda () (top-interp '{{f = {proc (x y z) {* {+ x y} z} }}
                                                {g = {proc (x z) {- z {l x x x}}}}
                                                {main = {proc () {+ {g 3 4} 3}}}})))

(check-exn #rx"QWJZ - Syntax Error: duplicate function name 'f"
           (lambda () (top-interp '{{f = {proc (x y) {+ x y}}}
                                    {f = {proc (n) {+ n 1}}}
                                    {main = {proc () {f 1 2}}}})))

(check-exn #rx"QWJZ - main should not have any arguments"
           (lambda () (top-interp '{{f = {proc (x y) {+ x y}}}
                                    {main = {proc (blah) {f 1 2}}}})))

(check-equal? (top-interp '{{f = {proc (x y z) {* {+ x y} z} }}
                             {g = {proc (x z) {- z {f x x x}}}}
                             {main = {proc () {+ {g 3 2} 3}}}})
              -13)

(check-equal? (top-interp '{{f = {proc (x y z) {* {+ x y} z} }}
                             {g = {proc (x z) {- z {f x x x}}}}
                             {main = {proc () {ifleq0? {+ {g 3 2} 3} 5 13}}}})
              5)








