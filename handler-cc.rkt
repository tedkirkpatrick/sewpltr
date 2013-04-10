#lang racket
(require redex)

(define-language handler-iswim
  ((M N L K) X (λ X M) (M M) b (o2 M M) (o1 M) (throw b) (catch M_1 with (λ X M_2)))
  (o o1 o2)
  (o1 add1 sub1 iszero)
  (o2 + - * **)
  (b number)
  ((V U W) b X (λ X M))
  (F hole (V F) (F M) (o V ... F M ...))
  (E hole (V E) (E M) (o V ... E M ...) (catch E with (λ X M)))
  ((X Y Z) variable-not-otherwise-mentioned))

(define-metafunction handler-iswim
  δ : M -> M
  [(δ (iszero 0)) (λ x (λ y x))]
  [(δ (iszero b)) (λ x (λ y y))]
  [(δ (add1 b)) ,(add1 (term b))]
  [(δ (sub1 b)) ,(sub1 (term b))]
  [(δ (+ b_1 b_2)) ,(+ (term b_1) (term b_2))]
  [(δ (- b_1 b_2)) ,(- (term b_1) (term b_2))]
  [(δ (* b_1 b_2)) ,(* (term b_1) (term b_2))]
  [(δ (** b_1 b_2)) ,(expt (term b_1) (term b_2))])

(define-metafunction handler-iswim
  ;subst : M X M -> M
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
  [(subst any_2 X_1 any_1) any_2]
  [(subst (catch M_1 with (λ X_2 M_2)) X_3 M_3)
   (catch (subst M_1 X_3 M_3) with (subst (λ X_2 M_2) X_3 M_3))]
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
   (--> ((V M) (E ...))
        (M (E ... (V hole)))
        cc2
        (side-condition
         (not (redex-match handler-iswim V (term M)))))
   (--> ((o V ... M N ...) (E ...))
        (M (E ... (o V ... hole N ...)))
        cc3
        (side-condition
         (not (redex-match handler-iswim V (term M)))))
   (--> (((λ X M) V) (E ...))
        ((subst M X V) (E ...))
        ccfiv
        (side-condition (begin (display (term M)) (display (term (E ...))) #t)))
   (--> ((o b ...) (E ...))
        ((δ (o b ...)) (E ...))
        ccffi)
   (--> (V (E ... (U hole)))
        ((U V) (E ...))
        cc4)
   (--> (V (E ... (hole N)))
        ((V N) (E...))
        cc5)
   (--> (V_n (E ... (o V ... hole N ...)))
        ((o V ... V_n N ...) (E ...))
        cc6)
         ))
(current-traced-metafunctions 'all)