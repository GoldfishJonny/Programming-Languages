#lang typed/racket

(require typed/rackunit)

;All error messages must contain the string "AAQZ"
;Error messages must be good enough to help the user understand what went wrong
;
;Dont mutate bindings and dont mutate structure fields
;You do not need to use hash tables
;if you do use hash tables use immutable hash tables only; no hash-set!.

(define-type ArithC (U numC plusC multC squareC))

(struct numC ([n : Real]) #:transparent)
(struct plusC ([l : ArithC] [r : ArithC]) #:transparent)
(struct multC ([l : ArithC] [r : ArithC]) #:transparent)
(struct squareC ([a : ArithC]) #:transparent)

; Extend the parser to handle a new ExprC 
; type that can interpret more kinds of expressions including
;  conditionals (ifleq0?) and binary operations (+, -, *, /).

(define-type ExprC
  (U numC
     binopC
     ifleq0C
     appC
     idC))

(struct binopC ([op : Symbol] [left : ExprC] [right : ExprC]) #:transparent)
(struct ifleq0C ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct appC ([func : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct idC ([name : Symbol]) #:transparent)


(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (numC n)]
    [(list '+ l r) (binopC '+ (parse l) (parse r))]
    [(list '* l r) (binopC '* (parse l) (parse r))]
    [(list '- l r) (binopC '- (parse l) (parse r))]
    [(list '/ l r) (binopC '/ (parse l) (parse r))]
    [(list 'ifleq0? test then else) (ifleq0C (parse test) (parse then) (parse else))]
    [(list f args ...) (appC f (map parse args))]
    [(? symbol? n) (idC n)]
    [_ (error 'parse "AAQZ: malformed input ~e" s)]))

(define (interp [e : ExprC] [funs : (Listof FundefC)]) : Real
  (match e
    [(numC n) n]
    [(binopC op l r) (error 'interp "AAQZ: binopC not implemented")]
    [(ifleq0C test then else) (error 'interp "AAQZ: ifleq0C not implemented")]
    [(appC f args) (error 'interp "AAQZ: appC not implemented")]
    [(idC n) (error 'interp "AAQZ: idC not implemented")]))

(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

; Test Cases
(check-equal? (top-interp '5) 5)
(check-equal? (top-interp '{+ 2 3}) 5)
