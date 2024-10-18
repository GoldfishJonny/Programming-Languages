#lang racket

(require rackunit)
; 3 goes in x making it 3 + 2 = 5
((lambda (x) (+ x 2)) 3)

; second lambda is f
; third lambda is g
; third goes in g making 6
; second goes in f making 6 + 3 = 9
((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))

; receives a number and returns a function that adds to that number
(define (curried-add a)
  (lambda (b) (+ a b)))

; Test Cases
(check-equal? ((curried-add 3) 2) 5)
(check-equal? ((curried-add 2) 2) 4)

; receives a function that requires 2 inputs and returns a function
(define (curry2 f)
  (lambda (a) (lambda (b)
                (f a b))))

; Test Cases
(define test1 (lambda (x y) (+ x y)))
(define test2 (lambda (a b) (* a b)))
(check-equal? (((curry2 test1) 1)2) 3)
(check-equal? (((curry2 test2)3)3)9)

; recieves a function that requires 3 inputs and returns a function
(define (curry3 f)
  (lambda (a) (lambda (b)
                (lambda (c)
                  (f a b c)))))

; Test Cases
(define test3 (lambda (a b c) (- (+ a b) c)))
(define test4 (lambda (x y z) (* (+ x z) y)))
(check-equal? ((((curry3 test3)1)2)3)0)
(check-equal? ((((curry3 test4)1)2)3) 8)


; receives a list and symbol and
; returns true when it encounters symbol in list
(define (contains? lst sym)
  (match lst
    ['() #f]
    [(cons l r)
     (cond
       [(equal? l sym) #t]
       [else (contains? r sym)])]))

; Test Cases
(define test5 (list 'a 'b 'c 'hello 'h))
(check-equal? (contains? test5 'hello) #t)
(check-equal? (contains? test5 'yo) #f)

(define (in-list-many? source query)
  (map ((curry2 contains?) source) query))

; Test Cases
(define test6 (list 'a 'c 'hello 'yo 'hai))
(check-equal? (in-list-many? test5 test6) (list #t #t #t #f #f))
(define test7 (list 'a 'b 'c 'hello 'h))
(check-equal? (in-list-many? test5 test7)(list #t #t #t #t #t))