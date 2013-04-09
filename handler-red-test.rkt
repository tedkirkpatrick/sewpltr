#lang racket
(require redex)
(provide handler-red-test)

(define handler-red-test 
  (lambda (red)
    (test-->> red
              (term (add1 1))
              (term 2))
    (test-->> red
              (term (catch (throw 1) with (λ x x)))
              (term 1))
    (test-->> red
              (term (catch (+ 1 1) with (λ x 10)))
              (term 2))
    (test-->> red
              (term (catch (+ 1 (+ 2 (throw 5))) with (λ x x)))
              (term 5))
    (test-->> red
              (term (catch (+ 1 (+ 2 (+ 4 5))) with (λ x 222)))
              (term 12))
    (test-->> red
              (term (catch (+ 1 (+ 2 (catch (+ 1 (throw 44)) with (λ x x)))) with (λ x 222)))
              (term 47))
    (test-results)))