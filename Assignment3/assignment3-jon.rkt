#lang typed/racket

(require typed/rackunit)

;All error messages must contain the string "AAQZ"
;Error messages must be good enough to help the user understand what went wrong

; ExprC
(define-type ExprC (U numC plusC multC squareC))
(struct numC ([n : Real]) #:transparent)
(struct plusC ([l : ExprC] [r : ExprC]) #:transparent)
(struct multC ([l : ExprC] [r : ExprC]) #:transparent)
(struct squareC ([a : ExprC]) #:transparent)

; FundefC
(define-type
  [fdC (name : Symbol)
       (arg  : Symbol)
       (body : ExprC)])

; Converts a given S-expression into an ArithC expression
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (numC n)]
    [(list '+ l r) (plusC (parse l) (parse r))]
    [(list '* l r) (multC (parse l) (parse r))]
    [(list '^2 a) (squareC (parse a))]
    [other (error 'parse "AAQZ poor formatting, got ~e" s)]))

; Evaluates the given ExprC expression and returns its numeric result.
(define (interp [a : ExprC]) : Real
  (match a
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]
    [(squareC a) (define x (interp a)) (* x x)]))

;
;(define (top-interp [fun-sexps : Sexp]) : Real
 ; (interp-fns (parse-prog fun-sexps)))

;(define (interp-fns (funs : (listof FundefC))) : Real
 ; )

;(define (parse-prog [fun-sexps : Sexp]) : (Listof FundefC)
 ; )
;(define ())