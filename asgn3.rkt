#lang typed/racket
 
(require typed/rackunit)

;; Full project implemented


;; ExprC Data Definition
(define-type ExprC (U BinOp NumC))
(struct NumC ([n : Real]) #:transparent)

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
    [other (error 'parse "QWJZ - expected valid expression, got ~e" other)]))

;; interp
(define (interp [a : ExprC]) : Real
  (match a
    [(NumC n) n]
    [(PlusC l r) (+ (interp l) (interp r))]
    [(MultC l r) (* (interp l) (interp r))]))

;; Accepts an s-expression and calls the parser and then the interp function.
(define (top-interp [exp : Sexp]) : Real
  (interp (parse exp)))

;; Test Cases

;; top-interp tests
(check-equal? (top-interp '{+ 1 2}) 3)
(check-equal? (top-interp '2) 2)
(check-equal? (top-interp '{* {+ 1 2} 3}) 9)