#lang racket
(require redex)
(require "iswim.rkt")
(require "set.rkt")
(require "store.rkt")
(require "basic-iswim-test.rkt")

(define-extended-language state-iswim
  iswim
  ((M N L K) .... (set X M))
  (E .... (set X E))
  (St Uninit S)
  )

(define-metafunction/extension FV state-iswim
  [(FVs (set X M)) ,(set-union (list (term (FVs M)) (set-singleton (term X))))]
  )

(define-metafunction state-iswim
  [(AV X) ,(set-empty)]
  [(AV (λ X M)) ,(set-diff (term (AV M)) (set-singleton (term X)))]
  [(AV (M N)) ,(set-union (term ((AV M) (AV N))))]
  [(AV (set X M)) ,(set-union (list (term (AV M)) (set-singleton (term X))))]
  [(AV b) ,(set-empty)]
  [(AV (o M ...)) ,(set-union (term ((AV M) ...)))]
  )

(define Store (store-make))

(define cs-red
  (reduction-relation
   state-iswim
   (--> (any Uninit)
        (any S)
        init-store
        (side-condition
         (begin (set! Store (store-make)) #t)))
   (--> ((in-hole E ((λ X M) V)) S)
        ((in-hole E (subst M X V)) S)
        csfiv
        (side-condition
         (not (set-lookup (term X) (term (AV M))))))
   (--> ((in-hole E ((λ X M) V)) S)
        ((in-hole E (subst M X Y)) S)
        csfis
        (side-condition
         (set-lookup (term X) (term (AV M))))
        (where Y ,(variable-not-in
                   (term 
                     (,(store-vars Store)
                      (FVs (in-hole E M))))
                     (term X)))
        (side-condition
         (begin (store-update! (term Y) (term V) Store) #t)))
   (--> ((in-hole E X) S)
        ((in-hole E ,(store-lookup (term X) Store)) S)
        cs!)
   ; Following does not implement defined semantics:
   ; (set x v) returns v, not original value of x
   (--> ((in-hole E (set X V)) S)
        ((in-hole E ,(store-lookup (term X) Store)) S)
        cseq
        (side-condition
         (begin (store-update! (term X) (term V) Store) #t)))
   (--> ((in-hole E (o V ...)) S)
        ((in-hole E (δ (o V ...))) S)
        csffi)
   ))

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

(define (run-cs-test tm val)
  (run-test cs-red tm val))

(define (run-test red tm val)
  (let [(res (apply-reduction-relation* red
                                        (term (,tm Uninit))))
        (same? (lambda (res)
                 (and
                  (equal? (length res) 1)
                  (equal? (caar res) val))))]
    
    (test-predicate same? res)))

(define (test-basics)
  (run-basic-tests run-cs-test))

