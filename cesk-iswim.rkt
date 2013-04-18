#lang racket
(require redex)

(require "set.rkt")
(require "store.rkt")

(require "state-iswim.rkt")

(define-extended-language cesk-iswim
  state-iswim
  (Cl (V ε)) ; Closure
  (κ mt (fn Cl κ) (ar Cl κ) (op (V ... o) (Cl ...) κ) (set ς κ))
  (Ev (X ς))
  (ε (Ev ...))
  (ς variable-not-otherwise-mentioned)
  )

(define-metafunction cesk-iswim
  [(update (Ev ...) X ς) ((X ς) Ev ...)]
  )

(define-metafunction cesk-iswim
  [(lookup X ()) (0 ())]
  [(lookup X ((X ς) Ev ...)) ς]
  [(lookup X ((Y ς) Ev ...)) (lookup X (Ev ...))]
  )

(define Store (store-make))

(define (update-and-return-prior! var val store)
  (begin0
    (store-lookup var store)
    (store-update! var val store)))

(define cesk-red
  (reduction-relation
   cesk-iswim
   (--> ((any ε) Uninit κ)
        ((any ε) S κ)
        init-store
        (side-condition
         (begin (set! Store (store-make)) #t)))
   (--> (((M N) ε) S κ)
        ((M ε) S (ar (N ε) κ))
        cesk1)
   (--> (((o M N ...) ε) S κ)
        ((M ε) S (op (o) ((N ε) ...) κ))
        cesk2)
   (--> ((V ε) S (fn ((λ X M) ε_f) κ))
        ((M (update ε X ς_n)) S κ)
        cesk3
        (side-condition
         (not (redex-match cesk-iswim X (term V))))
        (fresh ς_n)
        (side-condition
         (begin (store-update! (term ς_n) (term (V ε)) Store) #t)))
   (--> ((V ε_v) S (ar (N ε_n) κ))
        ((N ε_n) S (fn (V ε_v) κ))
        cesk4
        (side-condition
         (not (redex-match cesk-iswim X (term V)))))
   (--> ((X ε) S κ)
        (,(store-lookup (term (lookup X ε)) Store) S κ)
        cesk7)
   ))




