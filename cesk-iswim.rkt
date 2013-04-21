#lang racket
(require redex)

(require "set.rkt")
(require "store.rkt")

(require "state-iswim.rkt")

(define-extended-language cesk-iswim
  state-iswim
  (Cl (V ε)) ; Closure
  (κ mt (fn (M ε) κ) (ar (M ε) κ) (op (Cl ... o) ((M ε) ...) κ) (set σ κ) (frame X κ))
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

; Implementation of store

(define Store (store-make))

(define (update-and-return-prior! var val store)
  (begin0
    (store-lookup var store)
    (store-update! var val store)))

; Mechanism to generate fresh storage locations.
; Can't use redex "fresh" clause of a reduction relation because that reuses
; names when they have left scope ("fresh" only guarantees the name doesn't occur in
; the term currently begin reduced).
(define next-σ 0)
(define (fresh-σ)
  (term ,(begin0 (string->symbol (format "σ_~v" next-σ)) (set! next-σ (+ next-σ 1)))))

(define (init-store)
  (set! Store (store-make))
  (set! next-σ 0))

(define (unstack env context)
  (let [(match (redex-match cesk-iswim (frame X κ) context))
        (var-name (lambda (mls) (bind-name (car (match-bindings (car mls))))))
        (con-val (lambda (mls) (bind-exp (cadr (match-bindings (car mls))))))]
    (if match
        (if (equal? 1 (length match))
            (begin
;              (when (null? env)
;                (printf "Null environment ~s" match))             
              (begin (store-delete! (var-name match) Store) (con-val match)))
            (raise (list "Multiple matches for" context match)))
        context)))

; CESK reduction relation 

(define cesk-red
  (reduction-relation
   cesk-iswim
   (--> ((any ε) Uninit κ)
        ((any ε) S κ)
        init-store
        (side-condition
         (begin (init-store) #t)))
   (--> (((M N) ε) S κ)
        ((M ε) S (ar (N ε) κ))
        cesk1)
   (--> (((o M N ...) ε) S κ)
        ((M ε) S (op (o) ((N ε) ...) κ))
        cesk2)
   (--> ((V ε) S (fn ((λ X M) ε_f) κ))
        ((M (update ε_f X σ_n)) S (frame X ,(unstack (term ε) (term κ))))
        cesk3
        (where σ_n ,(fresh-σ))
        (side-condition
         (not (redex-match cesk-iswim X (term V))))
        (side-condition
         (begin (store-update! (term σ_n) (term (V ε)) Store) #t)))
   (--> ((V ε) S (frame X κ))
        ((V ε) S κ)
        pop-frame)
   (--> ((V ε_v) S (ar (N ε_n) κ))
        ((N ε_n) S (fn (V ε_v) κ))
        cesk4
        (side-condition
         (not (redex-match cesk-iswim X (term V)))))
   (--> ((b_m ε_m) S (op ((b ε) ... o) () κ)) ; LOSS (but environment irrelevant
        (((δ ,(reverse (term (b_m b ... o)))) ()) S ,(unstack (term ε_m) (term κ)))
        cesk5)
   (--> ((V ε_v) S (op (        Cl ... o) ((N ε_n) (M ε_m) ...) κ))
        ((N ε_n) S (op ((V ε_v) Cl ... o) (        (M ε_m) ...) κ))
        cesk6
        (side-condition
         (not (redex-match cesk-iswim X (term V)))))
   (--> ((X ε) S κ)  ; LOSS of environment for variable
        (,(store-lookup (term (lookup X ε)) Store) S ,(unstack (term ε) (term κ)))
        cesk7)
   (--> (((set X M) ε) S κ)
        ((M ε) S (set (lookup X ε) κ))
        cesk8)
   (--> (Cl S (set σ κ))
        (,(update-and-return-prior! (term σ) (term Cl) Store) S κ)
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
   ; Implement let in reduction relation
   (--> (((let () in M) ε) S κ)
        ((M ε) S κ)
        let-empty)
   (--> (((let ((X = M_v) (X_i = M_i) ...) in M) ε) S κ)
        ((((λ X (let ((X_i = M_i) ...) in M)) M_v) ε) S κ)
        let-n)
   ))

; ------ Testing  module -----

(module+ test
  (require "basic-iswim-test.rkt")
  (require "state-iswim-test.rkt")
  (provide test-basics test-state test-all)
    
  (define (run-test red tm val)
    (let [(res (apply-reduction-relation* red
                                          (term ((,tm ()) Uninit mt))))
          (same? (lambda (res)
                   (and
                    (equal? (length res) 1)
                    (list? (caar res))
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
    (printf "state:  ") (test-state))
  (test-all))



