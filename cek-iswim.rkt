#lang racket
(require redex)
(require "iswim.rkt")
(require "basic-iswim-test.rkt")

(define-extended-language cek-iswim
  iswim
  (Cl (M ε))  ; Closure
  (Val (V ε)) ; Closure of a value. In ISWIM, is ε guaranteed to be null?
  (κ mt (fn Cl κ) (ar Cl κ) (op (Val ... o) (Cl ...) κ))
  (Ev (X Cl))
  (ε (Ev ...))
  )

(define-metafunction cek-iswim
  [(update (Ev ...) X Val) ((X Val) Ev ...)]
  )

(define-metafunction cek-iswim
  [(lookup X ()) (0 ())]
  [(lookup X ((X Val) Ev ...)) Val]
  [(lookup X ((Y Val) Ev ...)) (lookup X (Ev ...))]
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
   (--> ((V ε) (fn ((λ X M) ε_x) κ))
        ((M (update ε_x X (V ε))) κ)
        cek3
        (side-condition
         (not (redex-match cek-iswim X (term V)))))
   (--> ((V ε) (ar (N ε_n) κ))
        ((N ε_n) (fn (V ε) κ))
        cek4
        (side-condition
         (not (redex-match cek-iswim X (term V)))))
   (--> ((b_m ε) (op ((b_m-1 ε_m-1)... o) () κ))
        (((δ ,(reverse (term (b_m b_m-1 ... o)))) ()) κ)
        cek5)
   (--> ((V ε_V) (op         (Cl_i ... o) ((N ε_N) Cl ...) κ))
        ((N ε_N) (op ((V ε_V) Cl_i ... o)         (Cl ...) κ))
        cek6
        (side-condition
         (not (redex-match cek-iswim X (term V)))))
   (--> ((X ε) κ)
        ((lookup X ε) κ)
        cek7)
   ))

; ------------ Testing tools -------------

(define (run-cek-test tm val)
  (let [(res (apply-reduction-relation* cek-red
                                        (term ((,tm ()) mt))))
        (same? (lambda (res)
                 (and
                  (equal? (length res) 1)
                  (equal? (caaar res) val))))]
    
    (test-predicate same? res)))

(define (test-basics)
  (run-basic-tests run-cek-test))
