#lang typed/racket

; Full project implemented.

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

; Returns the value of the Arithmetic Problem
(define (solve [op : Symbol] [l : Real] [r : Real]) : Real
  (match op
    ['+ (+ l r)]
    ['- (- l r)]
    ['* (* l r)]
    ['/
     (cond
       [(equal? r 0) (error 'solve "AAQZ: cannot divide by 0, got ~e/~e" l r)]
       [else (/ l r)])]))


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
       [else (error 'parse-fundef "AAQZ: bad syntax, got ~e" p)])]
    [_ (error 'parse-fundef "AAQZ: function header format is incorrect, got ~e" s)]))

; Parses the entire program, Returns list of FundefC
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    ['() '()]
    [(cons l r) (cons (parse-fundef l) (parse-prog r))]
    [_ (error 'parse-prog "AAQZ: invalid syntax, ~e" s)]))

; Substitute
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(numC n) in] ; numC
    [(binopC op l r)(binopC op (subst what for l) (subst what for r))] ; binopC
    [(idC n) ; idC
     (cond
       [(equal? n for) what]
       [else in])]
    [(appC fun arg) (appC fun (map (lambda ([a : ExprC]) : ExprC (subst what for a)) arg))] ; appC
    [(ifleq0C test then else) (ifleq0C (subst what for test) (subst what for then) (subst what for else))] ; ifleq0C
    ))

; Recursive Substitution
(define (subst-all [e : (Listof ExprC)] [param : (Listof Symbol)] [body : ExprC]) : ExprC
  (match (list e param)
    [(list '() '()) body]
    [(list (cons l r) (cons f b)) (subst-all r b (subst l f body))]))

; interp
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(numC n) n]
    [(binopC op l r) (solve op (interp l funs) (interp r funs))]
    [(appC fun arg)
     (define values (map (lambda ([a : ExprC]) : Real (interp a funs)) arg))
     (define parameters (map (lambda ([a : Real]) : ExprC (numC a)) values))
     (match (get-fundef fun funs)
       [(FundefC _ param body)
        (cond 
          [(equal? (length arg) (length param))
           (interp (subst-all parameters param body) funs)]
          [else (error 'interp "AAQZ: number of arguments != number of parameters, given ~e, ~e" values param)])])]
    [(ifleq0C test then else)
     (cond
       [(<= (interp test funs) 0) (interp then funs)]
       [else (interp else funs)])]
    [(idC n) (error 'interp "AAQZ: Error with subst, got ~e" n)]))

; interp-fns
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define main (get-fundef 'main funs))
    (interp (FundefC-body main) funs))

; top-interp
<<<<<<< jon
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))


;;; TEST CASES

; helper function valid-op?
(check-equal? (valid-op? 'def) #f)
(check-equal? (valid-op? '+) #t)
(check-equal? (valid-op? '-) #t)
(check-equal? (valid-op? '*) #t)
(check-equal? (valid-op? '/) #t)

;helper function valid-id?
(check-equal? (valid-id? '+) #f)
(check-equal? (valid-id? '=>) #f)
(check-equal? (valid-id? '/) #f)
(check-equal? (valid-id? 'helloWorld) #t)

; helper function duplicates?
(define dupe1 (list 'test 'test 'hello))
(define dupe2 (list 'test 'hello 'yo))
(check-equal? (duplicates? dupe1) #t)
(check-equal? (duplicates? dupe2) #f)

; helper function solve
(check-equal? (solve '+ 1 1) 2)
(check-equal? (solve '- 1 1) 0)
(check-equal? (solve '* 1 1) 1)
(check-equal? (solve '/ 1 1) 1)
(check-exn (regexp (regexp-quote "AAQZ: cannot divide by 0"))
           (λ () (solve '/ 1 0)))

; helper function get-fundef
(define list1 (list (FundefC 'main '() (numC 5)) (FundefC 'hello '(x) (numC 5))))
(check-equal? (get-fundef 'main list1) (FundefC 'main '() (numC 5)))
(check-exn (regexp (regexp-quote "AAQZ: function 'name not found"))
           (λ () (get-fundef 'name '())))

; parse function
(check-equal? (parse '{ifleq0? 0 5 1}) (ifleq0C (numC 0) (numC 5) (numC 1)))
(check-equal? (parse '{ifleq0? 1 5 1}) (ifleq0C (numC 1) (numC 5) (numC 1)))
(check-exn (regexp (regexp-quote "AAQZ: s-expression format is incorrect"))
           (λ () (parse '{5{}})))


; parse-fundef function
(define deffun : FundefC (FundefC 'helloWorld '(x) (binopC '+ (idC 'x) (numC 5))))
(check-equal? (parse-fundef '{def helloWorld {(x) =>{+ x 5}}}) deffun)
(check-equal? (parse-fundef '{def hello [() => {+ 5 5}]})
              (FundefC 'hello '() (binopC '+ (numC 5) (numC 5))))
(check-exn (regexp (regexp-quote "AAQZ: function header format is incorrect"))
           (λ () (parse-fundef '{def def '() => {x}})))
(check-exn (regexp (regexp-quote "AAQZ: invalid function name"))
           (λ () (parse-fundef '{def def { (x) => {x}}})))
(check-exn (regexp (regexp-quote "AAQZ: duplicate parameters"))
           (λ () (parse-fundef '{def hello { (x x) => {x}}})))

; parse-prog
(check-equal? (parse-prog '{{def main {() => {+ 5 5}}}}) (list (FundefC 'main '() (binopC '+ (numC 5) (numC 5)))))
(check-exn (regexp (regexp-quote "AAQZ: invalid syntax"))
           (λ () (parse-prog 5)))

; subst
(define orig-exp (list (idC 'x) (idC 'y)))
(define new-exp (list (numC 5) (idC 'y)))
(check-equal? (subst (numC 5) 'x (appC 'f orig-exp)) (appC 'f new-exp))

;interp
(check-exn (regexp (regexp-quote "AAQZ: Error with subst"))
           (lambda () (interp (appC 'hello (list (numC 25)))
                              (list (FundefC 'hello '(x) (binopC '+ (idC 'x) (idC 'y)))))))
; top-interp
(define prog1 '{
                {def hello {
                            (x y) => {+ y 5}}}
                {def main {
                           () => {hello 1 5}}}
                })
(define prog2 '{
                 {def hi {
                          (x y) => {+ y 5}}}
                 {def main {
                            () => {hi 1}}}})
(define prog3 '{
                {def yo {
                         (x y) => {+ {ifleq0? x 5 1} {ifleq0? y 5 2}}}}
                {def main {
                           () => {yo 0 1}}}})
(check-equal? (top-interp prog1) 10)
(check-exn (regexp (regexp-quote "AAQZ: number of arguments != number of parameters"))
           (λ () (top-interp prog2)))
(check-equal? (top-interp prog3) 7)