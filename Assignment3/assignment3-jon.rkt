#lang typed/racket

(require typed/rackunit)

;All error messages must contain the string "AAQZ"
;Error messages must be good enough to help the user understand what went wrong

; ExprC
(define-type ExprC (U numC binopC appC idC ifleq0C))
(struct numC ([n : Real]) #:transparent)
(struct binopC ([operator : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct appC ([fns : Symbol] [arg : (Listof ExprC)]) #:transparent)
(struct ifleq0C ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct idC ([n : Symbol]) #:transparent)

; FundefC
(struct FundefC ([name : Symbol] [param : (Listof Symbol)] [body : ExprC]) #:transparent)

; Returns true if given valid operations
(define (valid-op? [s : Sexp]) : Boolean
  (match s
    [(or '+ '- '* '/) #t]
    [_ #f]))

(define (valid-id? [a : Any]) : Boolean
  (match a
    [(or '+ '- '* '/ 'def 'quote) #f]
    [_ #t]))

; Retrieves the function definition
(define (get-fundef [name : Symbol] [funs : (Listof FundefC)]) : FundefC
  (match funs
    ['() (error 'get-fundef "AAQZ: function ~e not found" name)]
    [(cons (FundefC fun arg body) rest)
     (cond
       [(symbol=? fun name) (FundefC name arg body)] 
       [else (get-fundef name rest)])]))

; Converts a given S-expression into an ArithC expression
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (numC n)] ; numC
    [(list (? valid-op? binop) l r) (binopC (cast binop Symbol) (parse l) (parse r))] ; binopC
    [(list (and (? valid-id? fns) (? symbol? fns)) args ...) ; appC
     (define arg (map (lambda ([a : Sexp]) : ExprC (parse a)) args)) 
     (appC fns arg)]
    [(list 'ifleq0? if then else) (ifleq0C (parse if) (parse then) (parse else))] ; ifleq0C
    [(and (? valid-id? sym) (? symbol? sym)) (idC sym)] ; idC
    [other (error 'parse "AAQZ: s-expression format is incorrect, got ~e" s)]))

; Parses a function definition
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'def (? symbol? sym) '() '=> body) (FundefC sym '() (parse body))]
    [(list 'def (? symbol? sym) (list (list (? symbol? param) ...) '=> body))
     (FundefC sym (cast param (Listof Symbol)) (parse body))]
    [_ (error 'parse-fundef "AAQZ: function header format is incorrect, got ~e" s)]))

; Parses the entire program
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]
    [_ (error 'parse-prog "AAQZ: invalid syntax, ~e" s)]))


; Evaluates the given ExprC expression and returns its numeric result.
;(define (interp [a : ExprC]) : Real
;  (match a
;    [(numC n) n]
;    [(plusC l r) (+ (interp l) (interp r))]
;    [(multC l r) (* (interp l) (interp r))]
;    [(squareC a) (define x (interp a)) (* x x)]))

;()

;(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
;  )

;(define (interp-fns (funs : (listof FundefC))) : Real
;  ())

;(define (parse-prog [fun-sexps : Sexp]) : (Listof FundefC)
;  )

; top-interp
;(define (top-interp [fun-sexps : Sexp]) : Real
;  (interp-fns (parse-prog fun-sexps)))



; Test Cases parse-fundef
;(parse-fundef '{def helloWorld {(x) => {+ x 5}}})
(define deffun : FundefC (FundefC 'helloWorld '(x) (binopC '+ (idC 'x) (numC 5))))
(check-equal? (parse-fundef '{def helloWorld {(x) =>{+ x 5}}}) deffun)
(check-equal? (parse-fundef '{def hello [() => {+ 5 5}]}) (FundefC 'hello '() (binopC '+ (numC 5) (numC 5))))

; Test Cases get-fundef
(define test1 : FundefC (FundefC 'name '(x) (idC 'x)))
(check-exn (regexp (regexp-quote "AAQZ: function 'name not found"))
           (λ () (get-fundef 'name '())))
(check-equal? (get-fundef 'name (list test1)) test1)

; Test Cases parse-prog
(check-equal? (parse-prog '{{def main {() => {+ 5 5}}}}) (list (FundefC 'main '() (binopC '+ (numC 5) (numC 5)))))