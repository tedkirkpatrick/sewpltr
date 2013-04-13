#lang racket
(require "iswim.rkt")

(define coverage-values
  (lambda (test)
    (caching-enabled? #f)
    (current-traced-metafunctions 'all)
    (let (
          [subst-coverage (make-coverage subst)]
          [subst-var-coverage (make-coverage subst-var)])
      (relation-coverage (list subst-coverage subst-var-coverage))
      (let [(res (test))]
        (list
         (covered-cases subst-coverage)
         (covered-cases subst-var-coverage)
         ))
      )))

(define test
  (lambda ()
    (apply-reduction-relation/tag-with-names iswim-red
            (term ((λ x (λ y (+ y x))) 2)))))

(define test2
  (lambda ()
    (apply-reduction-relation/tag-with-names iswim-red
            (term ((λ x (λ y (+ y x))) x)))))

(define test3
  (lambda ()
    (apply-reduction-relation/tag-with-names iswim-red
            (term ((λ x (λ y (+ y x))) z)))))

; subst rules 3 4 and 5 invoked
(define test4
  (lambda ()
    (apply-reduction-relation/tag-with-names iswim-red
            (term ((λ y (+ y x)) y)))))

; subst rules 1 3 and 4 invoked
(define test5
  (lambda ()
    (apply-reduction-relation/tag-with-names iswim-red
            (term ((λ x ((λ x 1) x)) 2)))))
