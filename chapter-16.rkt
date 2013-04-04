#lang racket
(require redex)
(provide (all-defined-out) (all-from-out redex))

(define-language iswim
  ((M N L K) X (λ X M) (M M) b (o2 M M) (o1 M))
  (o o1 o2)
  (o1 add1 sub1 iszero)
  (o2 + - * **)
  (b number)
  ((V U W) b X (λ X M))
  (E hole (V E) (E M) (o V ... E M ...))
  ((X Y Z) variable-not-otherwise-mentioned))

(define-metafunction iswim
  [(δ (iszero 0)) (λ x (λ y x))]
  [(δ (iszero b)) (λ x (λ y y))]
  [(δ (add1 b)) ,(add1 (term b))]
  [(δ (sub1 b)) ,(sub1 (term b))]
  [(δ (+ b_1 b_2)) ,(+ (term b_1) (term b_2))]
  [(δ (- b_1 b_2)) ,(- (term b_1) (term b_2))]
  [(δ (* b_1 b_2)) ,(* (term b_1) (term b_2))]
  [(δ (** b_1 b_2)) ,(expt (term b_1) (term b_2))])

(define-metafunction iswim
  
  ;; 1. X_1 bound, so don't continue in λ body
  [(subst (λ X_1 any_1) X_1 any_2)
   (λ X_1 any_1)]
  
  ;; 2. do capture-avoiding substitution
  ;;    by generating a fresh name
  [(subst (λ X_1 any_1) X_2 any_2)
   (λ X_3
     (subst (subst-var any_1 X_1 X_3) X_2 any_2))
   (where X_3 (variable-not-in (term (X_2 any_1 any_2))
                                (term X_1)))]
  
  ;; 3. replace X_1 with any_1
  [(subst X_1 X_1 any_1) any_1]
  
  ;; the last two cases just recur on
  ;; the tree structure of the term
  [(subst (any_2 ...) X_1 any_1)
   ((subst any_2 X_1 any_1) ...)]
  [(subst any_2 X_1 any_1) any_2])

(define-metafunction iswim
  [(subst-var (any_1 ...) variable_1 variable_2)
   ((subst-var any_1 variable_1 variable_2) ...)]
  [(subst-var variable_1 variable_1 variable_2) variable_2]
  [(subst-var any_1 variable_1 variable_2) any_1])

(define iswim-red
  (reduction-relation
   iswim
   (--> (in-hole E ((λ X M) V))
        (in-hole E (subst M X V))
        βv)
   (--> (in-hole E (o b ...))
        (in-hole E (δ (o b ...)))
        δ)))

(define-extended-language iswim!
  iswim
  (P (V M))
  (M .... (get) (set M))
  (E .... (set E)))

(define red!
  (reduction-relation
   iswim!
   (--> (V_s (in-hole E ((λ X M) V)))
        (V_s (in-hole E (subst M X V)))
        βv)
   (--> (V_s (in-hole E (o b ...)))
        (V_s (in-hole E (δ (o b ...))))
        δ)
   (--> (V_s (in-hole E (get)))
        (V_s (in-hole E V_s))
        get)
   (--> (V_s (in-hole E (set V_n)))
        (V_n (in-hole E V_n))
        set)))

(define-extended-language sch
  iswim!
  (E hole (V E) (E M) (o V ... E V ...) (set E)))

(define-extended-language sch/nv
  sch
  (NV (side-condition M_1 (not (V? (term M_1))))))

(define V? (redex-match sch V))

(define sch3-red
  (extend-reduction-relation
   red!
   sch/nv
   (--> (in-hole E (o M_1 ... NV_1 M_2 ... NV_2 M_3 ...))
        (in-hole E ((λ X (o M_1 ... X M_2 ... NV_2 M_3 ...))
                    NV_1))
        left-lift
        (fresh X))
   (--> (in-hole E (o M_1 ... NV_1 M_2 ... NV_2 M_3 ...))
        (in-hole E ((λ X (o M_1 ... NV_1 M_2 ... X M_3 ...))
                    NV_2))
        right-lift
        (fresh X))))

(define main-example
  (term (0 (+ (set (+ (get) 1))
              (set (- (get) 1))))))