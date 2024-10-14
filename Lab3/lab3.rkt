#lang typed/racket

(require typed/rackunit)

;;; parse000 : Sexp -> Boolean

;; Example
; when (list 1 'chris 'a) is inputted, it returns true
; when (list 'a 'chris 'b) is inputted, it returns false
; when (list 1 'a 'a) is inputted, it returns false
; when (list 2 'chris 'b) is inputted, it returns true

; When a list is inputted with a number, 'chris and symbol it returns true
(define (parse000 [expression : Sexp ]) : Boolean
  (match expression
    [(list (? real?) 'chris (? symbol?)) #t]
    [other #f]
  ))

; Test Cases
(check-equal? (parse000 (list 1 'chris 'a)) #t)
(check-equal? (parse000 (list 'a 'chris 'b)) #f)
(check-equal? (parse000 (list 1 'a 'a)) #f)
(check-equal? (parse000 (list 2 'chris 'b)) #t)

;;; parse001

;; Example
; when (list 1 'chris 'a) is inputted, it returns 'a
; when (list 'a 'chris 'b) is inputted, it returns false
; when (list 1 'a 'a) is inputted, it returns false
; when (list 2 'chris 'b) is inputted, it returns 'b

;When a list is inputted with a number, 'chris and symbol it returns symbol else false
(define (parse001 [expression : Sexp]) : (U Boolean Symbol)
  (match expression
    [(list (? real?) 'chris (? symbol? sym)) sym]
    [other #f]))

; Test Cases
(check-equal? (parse001 (list 1 'chris 'a)) 'a)
(check-equal? (parse001 (list 'a 'chris 'b)) #f)
(check-equal? (parse001 (list 1 'a 'a)) #f)
(check-equal? (parse001 (list 2 'chris 'b)) 'b)

;;; parse002

;; Example
; when (list 'a (list 1 2 3) 'c) is inputted, it returns (list 1 2 3)
; when (list 1 (list a b c) '2) is inputted, it returns #f
; when (list (list 1 2 3) 'a 'c) is inputted, it returns #f
; when (list 'b 'b (list 1 2 3)) is inputted, it returns #f
; when (list 'b (list 1 2 'a) 'c) is inputted, it returns #f
; when (list 'c (list 1 2 3) 'a) is inputted, it returns (list 1 2 3)

;When a list in inputted with the second element being a list of number it returns the list else false
(define (parse002 [expression : Sexp]) : (U Boolean (Listof Real))
  (match expression
    [(list _ (list (? real? reals) ...) _) (cast reals (Listof Real))]
    [other #f]))
(check-equal?(parse002 (list 'a (list 1 2 3) 'c)) (list 1 2 3))
(check-equal? (parse002 (list 1 (list 'a 'b 'c) '2)) #f)
(check-equal? (parse002 (list (list 1 2 3) 'a 'c)) #f)
(check-equal? (parse002 (list 'b 'b (list 1 2 3))) #f)
(check-equal? (parse002 (list 'b (list 1 2 'a) 'c)) #f)
(check-equal? (parse002 (list 'c (list 1 2 3) 'a)) (list 1 2 3))
(check-equal? (parse002 '(3 (1.0 2.5 3.3) 4)) '(1.0 2.5 3.3))

;;; ohno

;; Example
; when 'a is inputted, it returns an error
; when 1 is inputted, it returns 'okay
; when (list 1 2 3) is inputted, it returns an error
; when #f is inputted, it returns an error

; Returns 'okay when a number is inputted, returns an error otherwise
(define (ohno [value : Any]) : Symbol
  (cond 
    [(real? value) 'okay]
    [else (error 'ohno "expected a number, got ~e" value)]))

(check-exn (regexp (regexp-quote "expected a number"))
           (λ () (ohno 'a)))
(check-equal? (ohno 1) 'okay)
(check-exn (regexp (regexp-quote "expected a number"))
           (λ () (ohno (list 1 2 3))))
(check-exn (regexp (regexp-quote "expected a number"))
           (λ () (ohno #f)))

;;; Arith Language Definition
(define-type ArithC (U numC plusC multC squareC))

(struct numC ([n : Real]) #:transparent)

(struct plusC ([l : ArithC] [r : ArithC]) #:transparent)

(struct multC ([l : ArithC] [r : ArithC]) #:transparent)

(struct squareC ([a : ArithC]) #:transparent)

;;; interp

;; Example
; when (numC 25) is inputted, it returns 25
; when (plusC (numC 25) (multC 2 1)) is inputted, it returns 27
; when (squareC 2) is inputted, it returns 4

(define (interp [a : ArithC]) : Real
  (match a
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]
    [(squareC a) (define x (interp a)) (* x x)]))

; Test Cases
(check-equal? (interp (numC 25)) 25)
(check-equal? (interp (plusC (numC 25) (multC (numC 2) (numC 1)))) 27)
(check-equal? (interp (squareC (numC 2))) 4)

;;; swap-adds

;; Example
; when (numC 25) is inputted, it returns (numC 25)
; when (plusC (numC 25) (numC 26)) is inputted, it returns (plusC (numC 26) (numC 25))
; when (multC (plusC (numC 1) (numC 2)) (numC 3)) is inputted
;     it returns (multC (plusC (numC 2) (numC 1)) (numC 3))
; when an ArithC is given and has plusC, the numbers will be swapped
(define (swap-adds [a : ArithC]) : ArithC
  (printf "~a\n" a)
  (match a
    [(numC n) (numC n)]
    [(plusC l r) (plusC (swap-adds r) (swap-adds l))]
    [(multC l r) (multC (swap-adds l) (swap-adds r))]
    [(squareC a) (squareC (swap-adds a))]))

; Test Cases
(check-equal? (swap-adds (numC 25)) (numC 25))
(check-equal? (swap-adds (plusC (numC 25) (numC 26))) (plusC (numC 26) (numC 25)))
(check-equal? (swap-adds (multC (plusC (numC 1) (numC 2)) (numC 3))) (multC (plusC (numC 2) (numC 1)) (numC 3)))


;;; parse

;; Example
; Arith = num
; | {+ Arith Arith}
; | {* Arith Arith}
; | {^2 Arith}

; when an s-expression is added it parses it as ArithC
(define (parse [s : Sexp]) : ArithC
  (match s
    [(? real? n) (numC n)]
    [(list '+ l r) (plusC (parse l) (parse r))]
    [(list '* l r) (multC (parse l) (parse r))]
    [(list '^2 a) (squareC (parse a))]
    [other (error 'parse "poor formatting, got ~e" s)]))

; Test Cases
(check-equal? (parse '5) (numC 5))
(check-equal? (parse '{^2 2}) (squareC (numC 2)))
(check-equal? (parse '{+ {* 2 1} 1}) (plusC (multC (numC 2) (numC 1)) (numC 1)))
(check-exn (regexp (regexp-quote "poor formatting"))
           (λ () (parse '{100 * 2})))

;;; top-interp

;; Example
; when '5 is inputted it returns 5
; when '{^2 2} is inputted it returns 4
; when '{+ {* 2 1} 1} is inputted it returns 3

;receives an s-expression and returns the result
(define (top-interp [s : Sexp]) : Real
  (interp (parse s)))

; Test Cases
(check-equal? (top-interp '5) 5)
(check-equal? (top-interp '{^2 2}) 4)
(check-equal? (top-interp '{+ {* 2 1} 1}) 3)

;;; zip

;; Example
; when lists '(1 2 3) and '(4 5 6): '((1 4) (2 5) (3 6))
; when lists '(1 2) and '(3 4) : '((1 3) (2 4))

(define (zip [l : (Listof Real)] [r : (Listof Real)]) : (Listof (Listof Real))
  (match (list l r)
    [(list '() '()) '()]
    [(list (cons f1 r1) (cons f2 r2))
     (cons (list f1 f2) (zip r1 r2))]))

; Test Cases
(check-equal? (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
(check-equal? (zip '(1 2) '(3 4)) '((1 3) (2 4)))
