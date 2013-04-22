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

(check-expect
 (begin (store-delete! (term a) st1) (store-vars st1))
 (list 'b))

(check-expect
 (with-handlers ([exn:fail? (lambda (v) #t)])
   (store-delete! (term ffff) st1))
 #t)

(test)