#lang typed/racket
 
(require typed/rackunit)

;; Full project implemented


;; ExprC Data Definition
(define-type ExprC (U NumC IfLeq0? BinOp Id Fcall))
(struct NumC ([n : Real]) #:transparent)
(struct IfLeq0? ([val : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
;(struct Op ([op : Symbol]) #:transparent)
(struct BinOp ([operator : Symbol] [l : ExprC] [r : ExprC])#:transparent)
(struct Id ([id : Symbol])#:transparent)
(struct Fcall ([name : Id] [args : (Listof ExprC)])#:transparent)


;; FundefC data definition
(struct FundefC ([name : Id] [args : (Listof Id)] [body : ExprC])#:transparent)




;; BinOp Data Definition (includes + - * /)
#;(define-type BinOp (U PlusC MultC SubC DivC))
(struct PlusC ([l : ExprC] [r : ExprC]) #:transparent)
(struct MultC ([l : ExprC] [r : ExprC]) #:transparent)
(struct SubC ([l : ExprC] [r : ExprC]) #:transparent)
(struct DivC ([l : ExprC] [r : ExprC]) #:transparent)




;; Parses an expression.
(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? n) (NumC n)]
    [(list (? is-op? n) left right) (BinOp (cast n Symbol) (parse left) (parse right))]
    [(? symbol? s) (Id s)]
    [(list 'ifleq0? val then else) (IfLeq0? (parse val) (parse then) (parse else))]
    [(list (? symbol? s) x ...) (Fcall (Id s) (map parse x))]
    [other (error 'parse "QWJZ - expected valid expression, got ~e" other)]))

;; is-op? checks if an sexp is an op

(define (is-op? [exp : Sexp]) : Boolean
  (or (equal? '+ exp) (equal? '- exp) (equal? '/ exp) (equal? '* exp)))

;; parse-fundef parses an s-exp into a funcion def

(define (parse-fundef [fundef : Sexp]) : FundefC
  (match fundef
    [(list (? symbol? name) '= (list 'proc (list (? symbol? id) ...) exp))
     (FundefC (Id name) (map (lambda ([x : Symbol]) : Id
                               (Id x)) (cast id (Listof Symbol))) (parse exp))]
    [other (error 'parse-fundef "QWJZ - wrong function definition format")]))



;; parse-prog take in s-exp (our program) returns listof(fundefs)

(define (parse-prog [prog : Sexp]) : (Listof FundefC)
  (match prog
    [(cons a b) (cons (parse-fundef a) (parse-prog b))]
    ['() '()]))

;; interp
(define (interp [a : ExprC]) : Real
  (match a
    [(NumC n) n]
    [(? BinOp? b) (do-binop b)]
    ;[(? Fcall? f) (function-lookup (Fcall-name f) )]
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


;; interp-fns , takes a list of fundefc's and evaluetes them

(define (interp-fns [funs : (Listof FundefC)]) : Real
  (function-sub (function-lookup (Id 'main) funs) '() funs))


;; function-lookup , given a name, the args, and a list of functions, find the function

(define (function-lookup [name : Id] [funs : (Listof FundefC)] ) : FundefC
  (match funs
    [(cons a b) (if (equal? (FundefC-name a) name) a (function-lookup name b))]
    [other (error 'function-lookup "QWJZ - function name ~e not found" (Id-id name))]))


;; function-sub , given a list of args and expressions, sub in 
;(define (function-sub))

(define (function-sub [func : FundefC] [vars : (Listof Real)] [funs : (Listof FundefC)]) : Real
  (match (FundefC-body func)
    [(BinOp o l r) (interp (BinOp o
                                  (NumC (function-sub (FundefC (FundefC-name func) (FundefC-args func) l) vars funs))
                                  (NumC (function-sub (FundefC (FundefC-name func) (FundefC-args func) r) vars funs))))]
    [(NumC n) (interp (NumC n))]
    [(IfLeq0? v t e) (interp (IfLeq0?
                              (NumC (function-sub (FundefC (FundefC-name func) (FundefC-args func) v) vars funs))
                              (NumC (function-sub (FundefC (FundefC-name func) (FundefC-args func) t) vars funs))
                              (NumC (function-sub (FundefC (FundefC-name func) (FundefC-args func) e) vars funs))))]
    [(Fcall n args1) (if (arg-verify args1 (FundefC-args (function-lookup n funs)))
                         (function-sub (function-lookup n funs) (arg-simplify args1 func vars funs) funs)
                         (error 'function-sub "QWJZ - wrong number of arguments pushed"))]
    [(Id s) (arg-find (Id s) vars (FundefC-args func)) ]))

;; arg-simplify, given arguments and conditions of calling function, evaluate them until they become reals

(define (arg-simplify [args : (Listof ExprC)] [func : FundefC] [vars : (Listof Real)] [funs : (Listof FundefC)]) : (Listof Real)
  (match args
    [(cons a b) (cons (function-sub (FundefC (FundefC-name func) (FundefC-args func) a) vars funs)
                      (arg-simplify b func vars funs))]
    ['() '()]))

;; arg-verify , given function args and function call, check that the right amount of args were pushed

(define (arg-verify [vars : (Listof ExprC)] [args : (Listof Id)]) : Boolean
  (= (length vars) (length args)))

;;arg-find , find the corresponding exprc to a symbol

(define (arg-find [find : Id] [vars : (Listof Real)] [args : (Listof Id)]) : Real
  (match* (vars args)
    [('() '()) (error 'interp "QWJZ - unbound identifer") ]
    [((cons f1 r1) (cons f2 r2)) (if (equal? find f2) f1 (arg-find find r1 r2))]))


;; Accepts an s-expression and calls the parser and then the interp function.
(define (top-interp [exp : Sexp]) : Real
  (interp (parse exp)))



;; Test Cases

;; top-interp tests
(check-equal? (top-interp '{+ 1 2}) 3) ;;should these be check-= tests instead??????
(check-equal? (top-interp '2) 2)
(check-equal? (top-interp '{* {+ 1 2} 3}) 9)
(check-equal? (top-interp '{/ 4 2}) 2)
(check-equal? (top-interp '{- 5 2}) 3)
(check-exn #rx"QWJZ - expected valid expression"
           (lambda () (top-interp "fail")))
(check-exn #rx"QWJZ - unbound identifer"
           (lambda () (top-interp '{bruh 5 2})))
(check-exn #rx"QWJZ - unbound identifer"
           (lambda () (top-interp '{fail})))
(check-exn #rx"QWJZ - unbound identifer"
           (lambda () (top-interp '{hi})))
;; ifleq0? tests
(check-equal? (parse '{ifleq0? 1 1 {- 1 1}}) (IfLeq0? (NumC 1) (NumC 1) (BinOp '- (NumC 1) (NumC 1))))
(check-equal? (top-interp '{ifleq0? 1 1 {- 1 1}}) 0) ; change this to variables
(check-equal? (top-interp '{ifleq0? -1 5 0}) 5)

;; test parsing of fcall

(check-equal? (parse '{foo 1 4 {+ 3 5}}) (Fcall (Id 'foo) (list (NumC 1) (NumC 4) (BinOp '+ (NumC 3) (NumC 5)))))

;(check-equal? (parse '{foo {+ 3 5}}) (Fcall (Id 'foo) (list (NumC 1) (NumC 4) (BinOp '+ (NumC 3) (NumC 5)))))
;; parsing fundef function

(check-equal? (parse-fundef '{foo = {proc {x y} {+ x y}}})
              (FundefC (Id 'foo) (list (Id 'x) (Id 'y)) (BinOp '+ (Id 'x) (Id 'y))))
(check-exn #rx"QWJZ - wrong function definition format"
           (lambda () (parse-fundef '{hi})))


;; testing for interp-fns
(check-equal? (interp-fns
       (parse-prog '{{f = {proc (x y) {+ x y}}}
                     {main = {proc () {f 1 2}}}}))
      3)


(check-equal? (interp-fns
        (parse-prog '{{f = {proc () 5}}
                      {main = {proc () {+ {f} {f}}}}}))
       10)


(check-equal? (interp-fns
        (parse-prog '{{f = {proc (x y z) {* {+ x y} z} }}
                      {g = {proc (x z) {- z x}}}
                      {main = {proc () {+ {f 3 2 4} {g 5 2}}}}}))
       17)

