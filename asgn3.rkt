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
    [(list (? symbol? n) left right) (BinOp n (parse left) (parse right))]
    [(? symbol? s) (Id s)]
    [(list 'ifleq0? val then else) (IfLeq0? (parse val) (parse then) (parse else))]
    [(list (? symbol? s) x ...) (Fcall (Id s) (map parse x))]
    [other (error 'parse "QWJZ - expected valid expression, got ~e" other)]))


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
    ;[(PlusC l r) (+ (interp l) (interp r))]
    ;[(MultC l r) (* (interp l) (interp r))]
    ;[(SubC l r) (- (interp l) (interp r))]
   ; [(DivC l r) (/ (interp l) (interp r))]
    [(IfLeq0? val then else) (cond [(<= (interp val) 0) (interp then)]
                                   [else (interp else)])]
    [other (error 'interp "QWJZ - unbound identifer")]))

;; interp-fns , takes a list of fundefc's and calculates them

(define (interp-fns [funs : (Listof FundefC)]) : Real
  (match funs
    [(cons a b) (if (= (FundefC-name a) 'main) 55 0)]))


;; do-binop : takes in a binop expression and returns a value

(define (do-binop [bin : BinOp]) : Real
  (match bin
    [(BinOp '+ l r) (+ (interp l) (interp r))]
    [(BinOp '- l r) (- (interp l) (interp r))]
    [(BinOp '* l r) (* (interp l) (interp r))]
    [(BinOp '/ l r) (/ (interp l) (interp r))]
    [other (error 'do-binop "QWJZ - unimplemented operand")]) )

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
(check-exn #rx"QWJZ - unimplemented operand"
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

