#lang typed/racket

(require typed/rackunit)

; ExprC
; Represents an expression in our language
(define-type ExprC (U numC idC appC lamC))
(struct numC ([n : Real]) #:transparent)
(struct idC ([n : Symbol]) #:transparent)
(struct appC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)
(struct lamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)

; Environment
; Represents an environment with bindings of variable names to values
(struct bindC ([name : Symbol] [val : Value]) #:transparent)
(define-type EnvC (Listof bindC))
(define mt-env '()) ; Empty environment
(define extend-env cons) ; Extend the environment

; Value
; Represents the different types of values in our language
(define-type Value (U numV closV boolV primV))
(struct numV ([n : Real]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct closV ([arg : (Listof Symbol)] [body : ExprC] [env : EnvC]) #:transparent)
(struct primV ([n : Symbol]) #:transparent)

; extend-all : (Listof Symbol) (Listof Value) EnvC -> EnvC
; Extends an environment with multiple variable bindings 
(define (extend-all[params : (Listof Symbol)] [args : (Listof Value)] [env : EnvC]) : EnvC
   (match (list params args)
    [(list '() '()) env]
    [(list (cons p ps) (cons a as)) (extend-env (bindC p a) (extend-all ps as env))]
    [_ (error 'extend-all "Mismatched number of parameters and arguments.")]))

; top-env : EnvC
; Defines the top-level environment with primitive bindings
(: top-env EnvC)
(define top-env
  (list
   (bindC 'true (boolV #true))
   (bindC 'false (boolV #false))
   (bindC '+ (primV '+))
   (bindC '- (primV '-))
   (bindC '* (primV '*))
   (bindC '/ (primV '/))
   (bindC '<= (primV '<=))
   (bindC 'equal? (primV 'equal?))))

; valid-op? : Sexp -> Boolean
; Returns true if the given symbol is a valid arithmetic operation
(: valid-op? (Sexp -> Boolean))
(define (valid-op? [s : Sexp]) : Boolean
  (match s
    [(or '+ '- '* '/) #t]
    [_ #f]))


; valid-id? : Any -> Boolean
; Returns true if the given value is a valid identifier (not a keyword)
(: valid-id? (Any -> Boolean))
(define (valid-id? [a : Any]) : Boolean
  (match a
    [(or 'def 'ifleq0? '=>) #f]
    [_ #t]))

; duplicates? : (Listof Symbol) -> Boolean
; Returns true if the list of symbols contains duplicates
(define (duplicates? [s : (Listof Symbol)]) : Boolean
  (match s
    ['() #f]
    [(cons l r) (if (member l r) #t (duplicates? r))]))

; solve : Symbol Value Value -> Value
; Solves an arithmetic or comparison operation based on the given operator and operands
(: solve (Symbol Value Value -> Value))
(define (solve [op : Symbol] [l : Value] [r : Value]) : Value
  (match (list op l r)
    [(list '+ (? numV? l) (? numV? r)) (numV (+ (numV-n l) (numV-n r)))]  ; Addition
    [(list '- (? numV? l) (? numV? r)) (numV (- (numV-n l) (numV-n r)))]  ; Subtraction
    [(list '* (? numV? l) (? numV? r)) (numV (* (numV-n l) (numV-n r)))]  ; Multiplication
    [(list '/ (? numV? l) (? numV? r))
     (cond
       [(= (numV-n r) 0) (error 'solve "AAQZ: cannot divide by 0, got ~e/~e" (numV-n l) (numV-n r))]
       [else (numV (/ (numV-n l) (numV-n r)))])]
    [(list '<= (? numV? l) (? numV? r)) (boolV (<= (numV-n l) (numV-n r)))]
    [(list 'equal? l r) (boolV (equal? l r))]
    [else (error 'solve "AAQZ: Unsupported operation or type mismatch ~e" op)]))


; parse : Sexp -> ExprC
; Parses an S-expression into an ExprC
(: parse (Sexp -> ExprC))
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (numC n)] ; numC
    [(list (list (? symbol? param) ...) '=> body)  ; lamC
     (lamC (cast param (Listof Symbol)) (parse body))]
    [(list fun args ...) ; appC
     (appC (parse fun) (map parse args))]
    [(? symbol? sym)
     (if (valid-id? sym)
         (idC sym)
         (error 'parse "AAQZ: Invalid identifier ~e" sym))]
    [else (error 'parse "AAQZ: s-expression format is incorrect, got ~e" s)]))

; lookup : Symbol EnvC -> Value
; Looks up the value of a symbol in the given environment
(: lookup (Symbol EnvC -> Value))
(define (lookup [for : Symbol] [env : EnvC]) : Value
  (match env
    ['() (error 'lookup "name not found: ~e" for)]
    [(cons (bindC name val) r)
     (cond
       [(symbol=? for name) val]
       [else (lookup for r)])]))

; serialize : Value -> String
; Serializes a value into a string representation
(: serialize (Value -> String))
(define (serialize [input : Value]) : String
  (match input
    [(numV n) (format "~v" n)]
    [(boolV b) (if b "true" "false")]
    [(closV _ _ _) "#<procedure>"]
    [(primV _) "#<primop>"]))

; interp : ExprC EnvC -> Value
; Evaluates an ExprC expression using the given environment
(: interp (ExprC EnvC -> Value))
(define (interp [exp : ExprC] [env : EnvC]) : Value
  (match exp
    [(numC n) (numV n)] ; numV
    [(idC n) (lookup n env)] ; idC
    [(lamC params body) (closV params body env)] ; funV
    [(appC fun args) ; appC
     (define fval (interp fun env))
     (define values (map (λ ([arg : ExprC]) : Value (interp arg env)) args))
     (match fval
       [(closV params body env-clos)
        (if (= (length params) (length values))
            (interp body (extend-all params values env-clos))
            (error 'interp "AAQZ: Incorrect number of arguments for lambda, expected ~a, got ~a" (length params) (length values)))]
       [(primV op)
        (match values
          [(list l r) (solve op l r)]
          [else (error 'interp "AAQZ: Incorrect number of arguments for operator, expected 2, got ~a" (length values))])]
       [else (error 'interp "AAQZ: Cannot apply non-function value ~e" fval)])]))



; top-interp
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))


(define prog1 '{{(z) => {+ z 3}} 2})
(define prog2 '{{(z y) => {+ z y}} {+ 9 14} 98})

(parse prog2)
(top-interp prog2)
