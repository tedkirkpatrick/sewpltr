#lang racket
(require redex)

(define-language iswim
  ((M N L K) X (λ X M) (M M) b (o2 M M) (o1 M) (err b))
  (o o1 o2)
  (o1 add1 sub1 iszero)
  (o2 + - * ** /)
  (b number)
  ((V U W) b X (λ X M) (err b))
  (E hole (V E) (E M) (o V ... E M ...))
  ((X Y Z) variable-not-otherwise-mentioned))

(define-metafunction iswim
  [(δ (iszero 0)) (λ x (λ y x))]
  [(δ (iszero b)) (λ x (λ y y))]
  [(δ (add1 b)) ,(add1 (term b))]
  [(δ (err b)) (err b)]
  [(δ (sub1 b)) ,(sub1 (term b))]
  [(δ (+ b_1 b_2)) ,(+ (term b_1) (term b_2))]
  [(δ (- b_1 b_2)) ,(- (term b_1) (term b_2))]
  [(δ (* b_1 b_2)) ,(* (term b_1) (term b_2))]
  [(δ (** b_1 b_2)) ,(expt (term b_1) (term b_2))]
  [(δ (/ b_1 0)) (err 0)]
  [(δ (/ b_1 b_2)) ,(/ (term b_1) (term b_2))]
  )

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
   (--> (in-hole E (err b))
        (err b)
        err
        (side-condition 
         (not (equal? (term hole) (term E)))))
   (--> (in-hole E (o V ... (λ X M_1) M_2 ...))
        (err 1)
        δerr)
   (--> (in-hole E (o b ...))
        (in-hole E (δ (o b ...)))
        δ
        (side-condition 
         (not (equal? (term o) (term err)))))))

(current-traced-metafunctions 'all)