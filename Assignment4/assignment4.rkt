#lang typed/racket

(require typed/rackunit)

; ExprC
(define-type ExprC (U numC idC appC lamC))
(struct numC ([n : Real]) #:transparent)
(struct idC ([n : Symbol]) #:transparent)
(struct appC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)
(struct lamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)

;Environment
(struct bindC ([name : Symbol] [val : Value]) #:transparent)
(define-type EnvC (Listof bindC))
(define mt-env '())
(define extend-env cons)

; Value
(define-type Value (U numV closV boolV primV))
(struct numV ([n : Real]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct closV ([arg : (Listof Symbol)] [body : ExprC] [env : EnvC]) #:transparent)
(struct primV ([n : Symbol]) #:transparent)

; Returns an EnvC that contains variables that are more than 1
(define (extend-all[params : (Listof Symbol)] [args : (Listof Value)] [env : EnvC]) : EnvC
   (match (list params args)
    [(list '() '()) env]
    [(list (cons p ps) (cons a as)) (extend-env (bindC p a) (extend-all ps as env))]
    [_ (error 'extend-all "Mismatched number of parameters and arguments.")]))

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

; Returns true if given valid operations
(define (valid-op? [s : Sexp]) : Boolean
  (match s
    [(or '+ '- '* '/) #t]
    [_ #f]))

; Returns true if given valid id
(define (valid-id? [a : Any]) : Boolean
  (match a
    [(or 'def 'ifleq0? '=>) #f]
    [_ #t]))

; Returns true if duplicates are found
(define (duplicates? [s : (Listof Symbol)]) : Boolean
  (match s
    ['() #f]
    [(cons l r) (if (member l r) #t (duplicates? r))]))

; Returns the value of the Arithmetic Problem
(define (solve [op : Symbol] [l : Value] [r : Value]) : Value
    (match (list op l r)
    [(list '+ (? numV? l) (? numV? r)) (numV (+ (numV-n l) (numV-n r)))]  ; Addition
    [(list '- (? numV? l) (? numV? r)) (numV (- (numV-n l) (numV-n r)))]  ; Subtraction
    [(list '* (? numV? l) (? numV? r)) (numV (* (numV-n l) (numV-n r)))]  ; Multiplication
    [(list '/ (? numV? l) (? numV? r))
     (cond
       [(equal? r 0) (error 'solve "AAQZ: cannot divide by 0, got ~e/~e" l r)]
       [else (numV (/ (numV-n l) (numV-n r)))])]
    [(list '<= (? numV? l) (? numV? r)) (boolV (<= (numV-n l) (numV-n r)))]
    [(list 'equal? l r) (boolV (equal? l r))]))

; Converts a given S-expression into an ArithC expression, Returns ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (numC n)] ; numC
    [(list (list (? symbol? param)...) '=> body)  ; lamC
     (define params (cast param (Listof Symbol)))    
     (lamC params (parse body))]
    [(list fun args ...); appC
     (define arg (map (lambda ([a : Sexp]) : ExprC (parse a)) args))
     (appC (parse fun) arg)]
    [(and (? valid-id? sym) (? symbol? sym)) (idC sym)] ; idC
    [other (error 'parse "AAQZ: s-expression format is incorrect, got ~e" s)]))

; lookup Symbol EnvC -> Real
(define (lookup [for : Symbol] [env : EnvC]) : Value
  (match env
    ['() (error 'lookup "name not found: ~e" for)]
    [(cons (bindC name val) r)
     (cond
       [(symbol=? for name) val]
       [else (lookup for r)])]))

; serialize Value -> String
(define (serialize [input : Value]) : String
  (match input
    [(numV n) (format "~v" n)]
    [(boolV b) (format "~v" b)]
    [(closV _ _ _) (format "#<procedure>")]))

; interp
(define (interp [exp : ExprC] [env : EnvC]) : Value
  (match exp
    [(numC n) (numV n)] ;numV
    [(idC n) (lookup n env)] ;idC
    [(lamC a b) (closV a b env)] ;funV
    [(appC fun args) ;appC
     (define fval (interp fun env))
     (define values (map (lambda ([arg : ExprC]) : Value (interp arg env)) args))
     (match fval
       [(closV _ param body)
        (interp
         (closV-body fval)
         (extend-all (closV-arg fval) values env))]
       [(primV n)
        (match values
          [(cons l1 (cons l2 r)) (solve n l1 l2)])])]))

; top-interp
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))


(define prog1 '{{(z) => {+ z 3}} 2})
(define prog2 '{{(z y) => {+ z y}} {+ 9 14} 98})

(parse prog2)
(top-interp prog2)
