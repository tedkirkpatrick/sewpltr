#lang racket
(require redex)
(require "iswim.rkt")
(require "basic-iswim-test.rkt")

(define-extended-language cek-iswim
  iswim
  (κ mt (fn (V ε) κ) (ar (M ε) κ) (op ((V ε) ... o) ((M ε) ...) κ))
  (ε ((X (M ε)) ...))
  )

(define-metafunction cek-iswim
  [(update ((U ε_u) ...) X (V ε_v)) ((V ε_v) (U ε_u) ...)]
  )

(define-metafunction cek-iswim
  [(lookup X ()) (0 ())]
  [(lookup X ((X (V ε_v)) (U ε_u) ...)) (V ε_v)]
  [(lookup X ((Y (V ε_v)) (U ε_u) ...)) (lookup X ((U ε_u) ...))]
  )

(define cek-red
  (reduction-relation
   cek-iswim
   (--> (((M N) ε) κ)
        ((M ε) (ar (N ε) κ))
        cek1)
   (--> (((o M N ...) ε) κ)
        ((M ε) (op (o) ((N ε) ...) κ))
        cek2)
   (--> ((V ε_v) (fn ((λ X M) ε_x) κ))
        ((M (update ε_x X (V ε_v))) κ)
        cek3
        (side-condition
         (not (equal? (term V) (term X)))))
   (--> ((X ε) κ)
        ((lookup ε X) κ)
        cek7)
   ))
