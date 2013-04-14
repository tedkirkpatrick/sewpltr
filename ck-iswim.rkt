#lang racket
(require redex)
(require "iswim.rkt")

(define-extended-language ck-iswim
  iswim
  (κ mt (fn V κ) (ar N κ) (op (V ... o) (N ...) κ))
  )

(define ck-red
  (reduction-relation
   ck-iswim
   (--> ((M N) κ)
        (M (ar N κ))
        ck1)
   (--> ((o M N ...) κ)
        (M (op (o) (N ...) κ))
        ck2)
   (--> (V (fn (λ X M) κ))
        ((subst M X V) κ)
        ck3)
   (--> (V (ar N κ))
        (N (fn V κ))
        ck4)
   (--> (b_1 (op (b_2 ... o) () κ))
        ((δ ,(reverse (term (b_1 b_2 ... o)))) κ)
        ck5)
   (--> (V (op   (U ... o) (N L ...) κ))
        (N (op (V U ... o)   (L ...) κ))
        ck6)
   ))
    
