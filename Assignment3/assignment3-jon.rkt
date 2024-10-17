#lang typed/racket

(require typed/rackunit)

;All error messages must contain the string "AAQZ"
;Error messages must be good enough to help the user understand what went wrong

; ExprC
(define-type ExprC (U numC plusC multC squareC appC idC))
(struct numC ([n : Real]) #:transparent)
(struct plusC ([l : ExprC] [r : ExprC]) #:transparent)
(struct multC ([l : ExprC] [r : ExprC]) #:transparent)
(struct binopC ([operator : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct squareC ([a : ExprC]) #:transparent)
(struct appC ([fns : Symbol] [arg : ExprC]) #:transparent)
(struct idC ([n : Symbol]) #:transparent)

; FundefC
(struct FundefC ([name : Symbol] [param : (Listof Symbol)] [body : ExprC]) #:transparent)

; Returns true if given valid operations
(define (valid-op? [s : Sexp]) : Boolean
  (match s
    [(or '+ '- '* '/) #t]
    [_ #f]))


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
    [(? symbol? id) (idC id)]
    [(? real? n) (numC n)]
    [(list '+ l r) (plusC (parse l) (parse r))]
    [(list '* l r) (multC (parse l) (parse r))]
    [(list '^2 a) (squareC (parse a))]
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

; Parses the entire program
;(define (parse-prog [s : Sexp]) : (Listof FundefC)
;  (match s
;    []))

; Evaluates the given ExprC expression and returns its numeric result.
;(define (interp [a : ExprC]) : Real
;  (match a
;    [(numC n) n]
;    [(plusC l r) (+ (interp l) (interp r))]
;    [(multC l r) (* (interp l) (interp r))]
;    [(squareC a) (define x (interp a)) (* x x)]))

; Updated interp for binopC, adding division by zero error
(define (interp [a : ExprC]) : Real
  (match a
    [(numC n) n] ; base case for numbers
    [(plusC l r) (+ (interp l) (interp r))] ; recursive case for addition
    [(multC l r) (* (interp l) (interp r))] ; recursive case for multiplication
    [(squareC a) (define x (interp a)) (* x x)] ; recursive case for squaring
    [(binopC op l r)
     (match op
       ['+ (+ (interp l) (interp r))]
       ['* (* (interp l) (interp r))]
       ['- (- (interp l) (interp r))]
       ['/ (if (zero? (interp r))
               (error 'interp "AAQZ: Division by zero error")
               (/ (interp l) (interp r)))]
       [else (error 'interp "AAQZ: Unsupported operator ~e" op)])])) ; handle unexpected operators


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
(define deffun : FundefC (FundefC 'helloWorld '(x) (plusC (idC 'x) (numC 5))))
(check-equal? (parse-fundef '{def helloWorld {(x) =>{+ x 5}}}) deffun)
(check-equal? (parse-fundef '{def hello [() => {+ 5 5}]}) (FundefC 'hello '() (plusC (numC 5) (numC 5))))

; Test Cases get-fundef
(define test1 : FundefC (FundefC 'name '(x) (idC 'x)))
(check-exn (regexp (regexp-quote "AAQZ function 'name not found"))
           (λ () (get-fundef 'name '())))
(check-equal? (get-fundef 'name (list test1)) test1)