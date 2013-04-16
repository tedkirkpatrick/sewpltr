#lang racket
(require redex)
(require "iswim.rkt")
(require "set.rkt")

(define-extended-language state-iswim
  iswim
  ((M N L K) .... (set X M))
  )

(define-metafunction/extension FV state-iswim
  [(FVs (set X M)) ,(set-union (term (FVs M)) (set-singleton (term X)))]
  )
(define-metafunction state-iswim
  [(AV X) ,(set-empty)]
  [(AV (λ X M)) ,(set-diff (term (AV M)) (set-singleton (term X)))]
  [(AV (M N)) ,(set-union (term ((AV M) (AV N))))]
  [(AV (set X M)) ,(set-union (list (term (AV M)) (set-singleton (term X))))]
  [(AV b) ,(set-empty)]
  [(AV (o M ...)) ,(set-union (term ((AV M) ...)))]
  )

; ------ Testing tools -----

(define (test-AV)
  ; Expressions with no assignable variables
  (test-equal (term (AV x)) (set-empty))
  (test-equal (term (AV (λ x (+ y z)))) (set-empty))
  (test-equal (term (AV ((+ 2 1) (x y)))) (set-empty))
  (test-equal (term (AV (set x (+ y z)))) (set-singleton (term x)))
  (test-equal (term (AV 0)) (set-empty))
  (test-equal (term (AV (+ x y))) (set-empty))
  
  ; Expressions with assignable variables
  (test-equal (term (AV (λ y (set x (add1 y))))) (set-singleton (term x)))
  (test-equal (term (AV (λ x (set x (add1 z))))) (set-empty))
  (test-equal (term (AV ((set x y) (set z q)))) (set-list (term (x z))))
  (test-results))

