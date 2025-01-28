#lang typed/racket
 
(require typed/rackunit)

;; Full project implemented


;; ExprC Data Definition
(define-type ExprC (U BinOp NumC IfLeq0?))
(struct NumC ([n : Real]) #:transparent)
(struct IfLeq0? ([val : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

;; BinOp Data Definition (includes + - * /)
(define-type BinOp (U PlusC MultC SubC DivC))
(struct PlusC ([l : ExprC] [r : ExprC]) #:transparent)
(struct MultC ([l : ExprC] [r : ExprC]) #:transparent)
(struct SubC ([l : ExprC] [r : ExprC]) #:transparent)
(struct DivC ([l : ExprC] [r : ExprC]) #:transparent)

;; Parses an expression.
(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? n) (NumC n)]
    [(list '+ left right) (PlusC (parse left) (parse right))]
    [(list '* left right) (MultC (parse left) (parse right))]
    [(list '- left right) (SubC (parse left) (parse right))]
    [(list '/ left right) (DivC (parse left) (parse right))]
    [(list 'ifleq0? val then else) (IfLeq0? (parse val) (parse then) (parse else))]
    [other (error 'parse "QWJZ - expected valid expression, got ~e" other)]))

;; interp
(define (interp [a : ExprC]) : Real
  (match a
    [(NumC n) n]
    [(PlusC l r) (+ (interp l) (interp r))]
    [(MultC l r) (* (interp l) (interp r))]
    [(SubC l r) (- (interp l) (interp r))]
    [(DivC l r) (/ (interp l) (interp r))]
    [(IfLeq0? val then else) (cond [(<= (interp val) 0) (interp then)]
                                   [else (interp else)])]))

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
(check-exn #rx"QWJZ - expected valid expression"
           (lambda () (top-interp '{fail})))
;; ifleq0? tests
(check-equal? (parse '{ifleq0? 1 1 {- 1 1}}) (IfLeq0? (NumC 1) (NumC 1) (SubC (NumC 1) (NumC 1))))
(check-equal? (top-interp '{ifleq0? 1 1 {- 1 1}}) 0) ; change this to variables
(check-equal? (top-interp '{ifleq0? -1 5 0}) 5)