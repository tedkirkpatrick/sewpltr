#lang racket
(require redex)

(require "set.rkt")
(require "store.rkt")

(require "state-iswim.rkt")

(define-extended-language cesk-iswim
  state-iswim
  (Cl (V ε)) ; Closure
  (κ mt (fn Cl κ) (ar Cl κ) (op (Cl ... o) ((M ε) ...) κ) (set σ κ))
  (Ev (X σ))
  (ε (Ev ...))
  (σ variable-not-otherwise-mentioned)
  )

(define-metafunction cesk-iswim
  [(update (Ev ...) X σ) ((X σ) Ev ...)]
  )

(define-metafunction cesk-iswim
  [(lookup X ()) (0 ())]
  [(lookup X ((X σ) Ev ...)) σ]
  [(lookup X ((Y σ) Ev ...)) (lookup X (Ev ...))]
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
        ((M (update ε_f X σ_n)) S κ)
        cesk3
        (side-condition
         (not (redex-match cesk-iswim X (term V))))
        (fresh σ_n)
        (side-condition
         (begin (store-update! (term σ_n) (term (V ε)) Store) #t)))
   (--> ((V ε_v) S (ar (N ε_n) κ))
        ((N ε_n) S (fn (V ε_v) κ))
        cesk4
        (side-condition
         (not (redex-match cesk-iswim X (term V)))))
   (--> ((b_m ε_m) S (op ((b ε) ... o) () κ))
        (((δ ,(reverse (term (b_m b ... o)))) ()) S κ)
        cesk5)
   (--> ((V ε_v) S (op         (Cl ... o)   ((N ε_n) (M ε_m) ...) κ))
        ((N ε_n) S (op ((V ε_v) Cl ... o)         ((M ε_m) ...) κ))
        cesk6
        (side-condition
         (not (redex-match cesk-iswim X (term V)))))
   (--> ((X ε) S κ)
        (,(store-lookup (term (lookup X ε)) Store) S κ)
        cesk7)
   (--> (((set X M) ε) S κ)
        ((M ε) S (set (lookup X ε) κ))
        cesk8)
   (--> (Cl S (set σ κ))
        (,(update-and-return-prior! (term σ) (term Cl) Store) S x)
        cesk9)
   ; Implement seq in reduction relation
   (--> (((seq M) ε) S κ)
        ((M ε) S κ)
        seq-last)
   (--> (((seq M M_i ...) ε) S κ)
        ((((λ Y (seq M_i ...)) M) ε) S κ)
        seq-n
        (fresh Y)
        (side-condition
         (<= 1 (length (term (M_i ...))))))
;   ; Implement let in reduction relation
;   (--> ((in-hole E (let () in M)) S)
;        ((in-hole E M) S)
;        let-empty)
;   (--> ((in-hole E (let ((X = M_v) (X_i = M_i) ...) in M)) S)
;        ((in-hole E ((λ X (let ((X_i = M_i) ...) in M)) M_v)) S)
;        let-n)
   ))

; ------ Testing  module -----

(module+ test
  (require "basic-iswim-test.rkt")
  (require "state-iswim-test.rkt")
    
  (define (run-test red tm val)
    (let [(res (apply-reduction-relation* red
                                          (term ((,tm ()) Uninit mt))))
          (same? (lambda (res)
                   (and
                    (equal? (length res) 1)
                    (equal? (caaar res) val))))]
      
      (test-predicate same? res)))
  
  (define (run-cesk-test tm val)
    (run-test cesk-red tm val))
  
  (define (test-basics)
    (run-basic-tests run-cesk-test))
  
  (define (test-state)
    (run-state-tests run-cesk-test))
  
  (define (test-all)
    (printf "basics: ") (test-basics)
    ;(printf "state:  ") (test-state))
  )
  (test-all))



