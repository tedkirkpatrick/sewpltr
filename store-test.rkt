#lang racket
(require test-engine/racket-tests)
(require redex)
(require "store.rkt")

(check-expect 
 (store-size (store-make))
 0)

(check-expect
 (store-lookup (term a) (store-make))
 #f)

(define st1 (store-make))
(store-update! (term a) (term (λ x x)) st1)

(check-expect
 (store-lookup (term a) st1)
 (term (λ x x)))

(check-expect
 (begin
   (store-update! (term b) (term (+ 1 1)) st1)
   (store-lookup (term b) st1))
 (term (+ 1 1)))

(check-expect
 (begin
   (store-update! (term a) (term (* 3 4)) st1)
   (store-lookup (term a) st1))
 (term (* 3 4)))

(check-expect
 (store-lookup (term c) st1)
 #f)

(define (symbols-to-strings syms)
  (map (lambda (s) (symbol->string s)) syms))

(check-expect
 (sort (symbols-to-strings (store-vars st1)) string<?)
 (list "a" "b"))

(define st (new Store%))

(check-expect
 (send st lookup (term x))
 #f)

(check-expect
 (begin
   (send st update! (term x) 5)
   (send st clear)
   (send st update! (term y) 5)
   (sort (symbols-to-strings (send st vars)) string<?))
 (list "y"))

(check-expect
 (begin
   (send st update! (term x) (term (+ 1 1)))
   (send st lookup (term x)))
 (term (+ 1 1)))

(check-expect
 (begin
   (send st update! (term y) (term (λ x x)))
   (send st size))
 2)

(check-expect
 (begin
   (send st update! (term x) (term ((λ x x) 1)))
   (send st lookup (term x)))
 (term ((λ x x) 1)))

(check-expect
 (sort (symbols-to-strings (send st vars)) string<?)
 (list "x" "y"))

(test)