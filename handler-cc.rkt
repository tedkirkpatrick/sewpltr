#lang racket
(require redex)
(require "cc-test.rkt")
(require "handler-red-test.rkt")

(define-language handler-iswim
  ((M N L K) X (λ X M) (M M) b (o2 M M) (o1 M) (throw b) (catch M_1 with (λ X M_2)))
  (o o1 o2)
  (o1 add1 sub1 iszero)
  (o2 + - * ** /)
  (b number)
  ((V U W) b X (λ X M))
  (Elt b X) ; For randomly-generated free variables
  (F hole (V F) (F M) (o V ... F M ...))
  (E hole (V E) (E M) (o V ... E M ...) (catch E with (λ X M)))
  ((X Y Z) variable-not-otherwise-mentioned))

(define-metafunction handler-iswim
  δ : M -> M
  [(δ (iszero 0)) (λ x (λ y x))]
  [(δ (iszero b)) (λ x (λ y y)) ]
  [(δ (add1 b)) ,(add1 (term b))]
  [(δ (sub1 b)) ,(sub1 (term b))]
  [(δ (+ b_1 b_2)) ,(+ (term b_1) (term b_2))]
  [(δ (- b_1 b_2)) ,(- (term b_1) (term b_2))]
  [(δ (* b_1 b_2)) ,(* (term b_1) (term b_2))]
  [(δ (** b_1 b_2)) ,(expt (term b_1) (term b_2))]
  [(δ (/ b 0)) (throw b)]
  [(δ (/ b_1 b_2)) ,(/ (term b_1) (term b_2))]
  ; For randomly-generated terms, all free variables are bound to 1
  [(δ X) 1]
  )

(define-metafunction handler-iswim
  ;subst : M X M -> M
  ;; 1. X_1 bound, so don't continue in λ body
  [(subst (λ X_1 any_1) X_1 any_2)
   (λ X_1 any_1)
   (side-condition (dbc "λ bound\n"))
   ]
  ;; 2. do capture-avoiding substitution
  ;;    by generating a fresh name
  ;;  (This does not appear to ever be executed.)
  [(subst (λ X_1 any_1) X_2 any_2)
   (λ X_3
     (subst (subst-var any_1 X_1 X_3) X_2 any_2))
   (where X_3 (variable-not-in (term (X_2 any_1 any_2))
                                (term X_1)))
   (side-condition (dbc "λ free\n"))
   ]
  ;; 3. replace X_1 with any_1
  [(subst X_1 X_1 any_1) any_1
    (side-condition (dbc "replace var\n"))]
  ;; 6. Substitute into a catch expression
  [(subst (catch M_1 with (λ X_2 M_2)) X_3 M_3)
   (catch (subst M_1 X_3 M_3) with (subst (λ X_2 M_2) X_3 M_3))]
  ;; 4 and 5. these cases just recur on
  ;; the tree structure of the term
  [(subst (any_2 ...) X_1 any_1)
   ((subst any_2 X_1 any_1) ...)(side-condition (dbc "list\n"))]
  [(subst any_2 X_1 any_1) any_2 (side-condition (dbc "list element\n"))]
  )

(define-metafunction handler-iswim
  [(subst-var (any_1 ...) variable_1 variable_2)
   ((subst-var any_1 variable_1 variable_2) ...)]
  [(subst-var variable_1 variable_1 variable_2) variable_2]
  [(subst-var any_1 variable_1 variable_2) any_1])

(define cc-red
  (reduction-relation
   handler-iswim
   (--> ((M N) (E ...))
        (M (E ... (hole N)))
        cc1
        (side-condition
         (not (redex-match handler-iswim V (term M)))))
   (--> (((λ X N) M) (E ...))
        (M (E ... ((λ X N) hole)))
        cc2
        (side-condition
         (not (redex-match handler-iswim V (term M)))))
   (--> ((b M) (E ...))
        ((throw b)(E ...))
        cc2-num)
   (--> ((X M) (E ...))
        ((throw 0) (E ...))
        cc2-var)
   (--> ((o V ... M N ...) (E ...))
        (M (E ... (o V ... hole N ...)))
        cc3
        (side-condition
         (not (redex-match handler-iswim V (term M)))))
   (--> (((λ X M) V) (E ...))
        ((subst M X V) (E ...))
        ccfiv
        )
   ; Look up randomly-generated free variables
   (--> ((o b ... X Elt ...) (E ...))
        ((o b ... (δ X) Elt ...) (E ...))
        fvfill)
   (--> ((o Elt ... (λ X M) N ...) (E ...))
        ((throw ,(length (term (Elt ...)))) (E ...))
        opargerr)
   (--> ((o b ...) (E ...))
        ((δ (o b ...)) (E ...))
        ccffi)
   (--> (V (E ... (U hole)))
        ((U V) (E ...))
        cc4)
   (--> (V (E ... (hole N)))
        ((V N) (E ...))
        cc5)
   (--> (V_n (E ... (o V ... hole N ...)))
        ((o V ... V_n N ...) (E ...))
        cc6)
;   (--> (M (E ...))
;        ((throw b) (E ...))
;        cc7
;        (side-condition
;         (equal? (term (δ M)) (term (throw b)))))
   (--> ((throw b) (E ... F))
        ((throw b) (E ...))
        cc8)
   (--> ((catch M with (λ X N)) (E ...))
        (M (E ... (catch hole with (λ X N))))
         cc9)
   (--> (V (E ... (catch hole with (λ X N))))
        (V (E ...))
        cc10)
;   (--> ((throw b) (E ... (catch F with (λ X N))))
;        ((throw b) (E ... (catch hole with (λ X N))))
;        cc11
;        (side-condition
;         (not (redex-match handler-iswim F (term hole)))))
   (--> ((throw b) (E ... (catch hole with (λ X N))))
        (((λ X N) b) (E ...))
        cc12)
   ))

; ------------  Testing tools --------------

(define dbc-print #f)

(define dbc
  (lambda (tag)
    (begin (when dbc-print (display tag)) #t)))
              
(define test-basics
  (lambda () (run-tests cc-red)))

(define (test-catch)
  (handler-red-test cc-red cc-test))

(define (test-all)
  (test-basics)
  (test-catch))

(define coverage-values
  (lambda (test)
    (let ([handler-coverage (make-coverage cc-red)]
          [δ-coverage (make-coverage δ)]
          [subst-coverage (make-coverage subst)]
          [subst-var-coverage (make-coverage subst-var)])
      (relation-coverage (list handler-coverage δ-coverage subst-coverage subst-var-coverage))
      (test)
      (list
       (covered-cases handler-coverage)
       (covered-cases δ-coverage)
       (covered-cases subst-coverage)
       (covered-cases subst-var-coverage)
       )
      )))

(define cover-basics (lambda() (coverage-values test-basics)))
(define cover-catch (lambda () (coverage-values test-catch)))
(define cover-all (lambda () (coverage-values test-all)))

(define saved (term ())) ; will be overridden

(define (single-test tm)
  (caching-enabled? #f)
  (current-traced-metafunctions 'all)
  (let [(res (apply-reduction-relation/tag-with-names cc-red tm))]
    (set! saved res)
    res))

(define tt1
  (lambda ()
    (single-test (term (((λ x (λ y (+ y x))) 2) ((hole 3)))))))

(define (tt2)
  (single-test (term (((λ i (+ (throw 0) k)) (- s (λ T F))) ()))))

(define (apply*? red tm)
  (let [(res (apply-reduction-relation* red tm))]
    (cond [(and
            (equal? (length res) 1)
            (or
              (redex-match handler-iswim V (caar res))
              (redex-match handler-iswim (throw b) (caar res)))
            (null? (cadar res))) res]
          [else #f])))

(define (generated-tm? red size)
  (let [(tm (generate-term handler-iswim M size))]
    (printf "~v\n" tm)
    (set! saved tm)
    (apply*? red (term (,tm ())))))

(define (traces-last) (traces cc-red (term (,saved ()))))

; A language without throws (for testing)
(define-extended-language throwless handler-iswim
  ((M N L K) X (λ X M) (M M) b (o2 M M) (o1 M)))

(define (generated-nothrow? red size)
  (let [(tm (generate-term throwless M size))]
    (printf "~v\n" tm)
    (set! saved tm)
    (apply*? red (term (,tm ())))))
