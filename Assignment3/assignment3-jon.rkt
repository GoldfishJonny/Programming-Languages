#lang typed/racket

(require typed/rackunit)

; ExprC
(define-type ExprC (U numC binopC appC idC ifleq0C))
(struct numC ([n : Real]) #:transparent)
(struct binopC ([operator : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct appC ([fun : Symbol] [arg : (Listof ExprC)]) #:transparent)
(struct ifleq0C ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct idC ([n : Symbol]) #:transparent)

; FundefC
(struct FundefC ([name : Symbol] [param : (Listof Symbol)] [body : ExprC]) #:transparent)

; Returns true if given valid operations
(define (valid-op? [s : Sexp]) : Boolean
  (match s
    [(or '+ '- '* '/) #t]
    [_ #f]))

; Returns true if given valid id
(define (valid-id? [a : Any]) : Boolean
  (match a
    [(or '+ '- '* '/ 'def 'ifleq0? '=>) #f]
    [_ #t]))

; Returns true if duplicates are found
(define (duplicates? [s : (Listof Symbol)]) : Boolean
  (match s
    ['() #f]
    [(cons l r) (if (member l r) #t (duplicates? r))]))

; Retrieves the function definition
(define (get-fundef [name : Symbol] [funs : (Listof FundefC)]) : FundefC
  (match funs
    ['() (error 'get-fundef "AAQZ: function ~e not found" name)]
    [(cons (FundefC fun arg body) rest)
     (cond
       [(symbol=? fun name) (FundefC name arg body)] 
       [else (get-fundef name rest)])]))

; Converts a given S-expression into an ArithC expression, Returns ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (numC n)] ; numC
    [(list (? valid-op? binop) l r) (binopC (cast binop Symbol) (parse l) (parse r))] ; binopC
    [(list (and (? valid-id? fun) (? symbol? fun)) args ...) ; appC
     (define arg (map (lambda ([a : Sexp]) : ExprC (parse a)) args)) 
     (appC fun arg)]
    [(list 'ifleq0? test then else) (ifleq0C (parse test) (parse then) (parse else))] ; ifleq0C
    [(and (? valid-id? sym) (? symbol? sym)) (idC sym)] ; idC
    [other (error 'parse "AAQZ: s-expression format is incorrect, got ~e" s)]))

; Parses a function definition, Returns FundefC
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'def (? symbol? sym) (list param '=> body))
     (define p : (Listof Symbol) (cast param (Listof Symbol)))
     (cond
       [(not (valid-id? sym)) (error 'parse-fundef "AAQZ: invalid function name, got ~e" sym)]
       [(not (duplicates? p)) (FundefC sym p (parse body))]
       [else (error 'parse-fundef "AAQZ: invalid syntax, got ~e" p)])]
    [_ (error 'parse-fundef "AAQZ: function header format is incorrect, got ~e" s)]))

; Parses the entire program, Returns list of FundefC
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    ['() '()]
    [(cons l r) (cons (parse-fundef l) (parse-prog r))]
    [_ (error 'parse-prog "AAQZ: invalid syntax, ~e" s)]))

; Substitute
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) :ExprC
  (match in
    [(numC n) in]
    [(binopC op l r)(binopC op (subst what for l) (subst what for r))]
    [(idC n)
     (cond
       [(equal? n for) what]
       [else in])]
    [(appC fun arg) (appC fun (map (lambda ([a : Sexp]) : ExprC (parse a)) arg))]
    [(ifleq0C test then else) (ifleq0C (subst what for test) (subst what for then) (subst what for else))]
    ))


;(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
;  )

;(define (interp-fns (funs : (listof FundefC))) : Real
;  ())


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