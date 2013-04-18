;
; Version of State ISWIM that wraps store accesses in metafunctions.
; This variant represents accesses to the store by the metafunctions Σ, Σ!, and dom.
; The approach proved partly successful.
;
; Strengths:
;   - The Σ (S[X <- V]) notation parallels the mathematical notation. The reduction
;     rules are terser.
; Weaknesses:
;   - Metafunctions don't play well with side-effects. Caching has to be turned off.
;   - The mathematical notation uses Σ(S[X <- V]) and Σ(S[X <- U]) to denote
;     different *functions*. But in this implementation, they denote distinct
;     functions that can never coexist. This limitation is ultimately responsible
;     for the (semi?)awkward distinction between Σ and Σ!. This is because the
;     mathematical notation simply forks a new function and passes it as the
;     distinct state, whereas this implementation has to explicitly sequence
;     when updates are done, in turn forcing versions of Σ that return different
;     results.
;
;  This last weakness might well be overcome by implementing Σ using functional
;  hash tables. It is not clear to me if a functional implementation would
;  permit metafunction caching or not.
;
#lang racket
(require redex)
(provide (all-defined-out))

(require "set.rkt")
(require "store.rkt")

(require "iswim.rkt")

(define-extended-language state-iswim
  iswim
  ((M N L K) .... (set X M) (seq M M_i ...) (let ((X = M_v) ...) in M))
  ; I do not put let and seq in E because the reduction rule converts them to more
  ; primitive forms, which in turn are in E
  (E .... (set X E))
  (V b (λ X M))
  ; The store and its various representations:
  ; Uninit      ---undefined initial state
  ; S           ---generic state
  ; S([X])      ---look up identifier X
  ; S([X <- V]) ---update value for X with V; return prior value
  (St Uninit S (S[X]) (S[X <- V]))
  )

(define-metafunction/extension FV state-iswim
  [(FVs (set X M)) ,(set-union (list (term (FVs M)) (set-singleton (term X))))]
  [(FVs (seq M ...)) ,(set-union (term ((FVs M) ...)))]
  [(FVs (let ((X = M_v) ...) in M))
   ,(set-diff (set-union (term ((FVs M_v) ... (FVs M)))) (set-list (term (X ...))))]
  )

(define-metafunction state-iswim
  [(AV X) ,(set-empty)]
  [(AV (λ X M)) ,(set-diff (term (AV M)) (set-singleton (term X)))]
  [(AV (M N)) ,(set-union (term ((AV M) (AV N))))]
  [(AV (set X M)) ,(set-union (list (term (AV M)) (set-singleton (term X))))]
  [(AV b) ,(set-empty)]
  [(AV (o M ...)) ,(set-union (term ((AV M) ...)))]
  [(AV (seq M ...)) ,(set-union (term ((AV M) ...)))]
  [(AV (let ((X = M_v) ...) in M)) ,(set-union (term ((AV M_v) ... (AV M))))]
  )

; The global store that is created and updated by Σ and Σ!
(define store (new Store%))

; Sigh. Sigh. Because I am creating metafunctions with side-effects,
; I have to turn metafunction caching off.
; This is likely a killer for the entire store-as-metafunction approach,
; but I'm going to do it now just to see how the design works out
; otherwise.
(caching-enabled? #f)

; Σ returns a value in the store, possibly updating it
(define-metafunction state-iswim
  [(Σ (S[X])) ,(send store lookup (term X))]
  [(Σ (S[X <- V])) ,(begin0 (send store lookup (term X))
                            (send store update! (term X) (term V)))]
  )

; Σ! is only called to update the store. It always returns S.
(define-metafunction state-iswim
  [(Σ! Uninit) S (side-condition (begin (send store clear) #t))]
  [(Σ! (S[X <- V])) S (side-condition (begin (term (Σ (S[X <- V]))) #t))]
  )

; Domain of the store
(define-metafunction state-iswim
  dom : S -> (X ...)
  [(dom S) ,(send store vars)]
  )

(define cs-red
  (reduction-relation
   state-iswim
   (--> (any Uninit)
        (any (Σ! Uninit))
        init-store)
   (--> ((in-hole E ((λ X M) V)) S)
        ((in-hole E (subst M X V)) S)
        csfiv
        (side-condition
         (not (set-lookup (term X) (term (AV M))))))
   (--> ((in-hole E ((λ X M) V)) S)
        ((in-hole E (subst M X Y)) (Σ! (S[Y <- V])))
        csfis
        (side-condition
         (set-lookup (term X) (term (AV M))))
        (where Y ,(variable-not-in
                   (term 
                     ((dom S)
                      (FVs (in-hole E M))))
                     (term X))))
  (--> ((in-hole E X) S)
        ((in-hole E (Σ (S[X]))) S)
        cseq)
   (--> ((in-hole E (set X V)) S)
        ((in-hole E (Σ (S[X <- V]))) S)
        cs!)
   (--> ((in-hole E (o V ...)) S)
        ((in-hole E (δ (o V ...))) S)
        csffi)
   ; Implement seq in reduction relation
   (--> ((in-hole E (seq M)) S)
        ((in-hole E M) S)
        seq-last)
   (--> ((in-hole E (seq M M_i ...)) S)
        ((in-hole E ((λ Y (seq M_i ...)) M)) S)
        seq-n
        (fresh Y)
        (side-condition
         (<= 1 (length (term (M_i ...))))))
   ; Implement let in reduction relation
   (--> ((in-hole E (let () in M)) S)
        ((in-hole E M) S)
        let-empty)
   (--> ((in-hole E (let ((X = M_v) (X_i = M_i) ...) in M)) S)
        ((in-hole E ((λ X (let ((X_i = M_i) ...) in M)) M_v)) S)
        let-n)
   ))

; ------ Testing  submodule -----

(module+ test
  (require "basic-iswim-test.rkt")
  (require "state-iswim-test.rkt")
  
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
    
    ; seq and let
    (test-equal (term (AV (let ((x = 1) (y = 2)) in (seq (+ 1 2))))) (set-empty))
    (test-equal (term (AV (let ((x = 1) (y = 2)) in (seq (set x (+ 1 2)) (set y 5)))))
                (set-list (term (x y))))
    (test-results))
  
  (define (run-test red tm val)
    (let [(res (apply-reduction-relation* red
                                          (term (,tm Uninit))))
          (same? (lambda (res)
                   (and
                    (equal? (length res) 1)
                    (equal? (caar res) val))))]
      
      (test-predicate same? res)))
  
  (define (run-cs-test tm val)
    (run-test cs-red tm val))
  
  (define (test-basics)
    (run-basic-tests run-cs-test))
  
  (define (test-state)
    (run-state-tests run-cs-test))
  
  (define (test-all)
    (printf "AV:     ") (test-AV)
    (printf "basics: ") (test-basics)
    (printf "state:  ") (test-state))
  
  (test-all))

