#lang racket
(require redex)

(provide run-tests)

(define cc-test
  (lambda (red tm val)
    (test-->> red
              (term (,tm ()))
              (term (,val ())))))

(define run-tests
  (lambda (red)
    (cc-test red (term (add1 1)) 2)
    (cc-test red (term (+ 1 4)) 5)
    (cc-test red (term ((λ x (sub1 x)) 4)) 3)
    (cc-test red (term (+ 1 ((λ x (add1 (add1 x))) 2))) 5)
    ; Test rule cc1
    (cc-test red (term (((λ x (λ y x)) 5) 3)) 5)
    ; Test rule cc2 and cc4
    (cc-test red (term ((λ x (add1 x)) (+ 2 3))) 6)
    ; Test operators (implemented in δ)
    (cc-test red (term ((((iszero 0) (λ x (+ x 4))) (λ x (+ x 5))) 1)) 5)
    (cc-test red (term ((((iszero 1) (λ x (+ x 4))) (λ x (+ x 5))) 1)) 6)
    (cc-test red (term (- 4 2)) 2)
    (cc-test red (term (* 4 2)) 8)
    (cc-test red (term (** 4 2)) 16)
    ; Test subst rules 
    (cc-test red (term (((λ x ((λ x 1) x)) 2))) 1)
    (cc-test red (term ((((λ x (λ y (+ y x))) 2) 3))) 5)
    (test-results)))
