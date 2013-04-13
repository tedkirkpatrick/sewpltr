#lang racket
(require redex)
(provide handler-red-test)

(define handler-red-test 
  (lambda (red test)
    (test red
          (term (add1 1))
          (term 2))
    (test red
          (term (catch (throw 1) with (λ x x)))
          (term 1))
    (test red
          (term (catch (+ 1 1) with (λ x 10)))
          (term 2))
    (test red
          (term (catch (+ 1 (+ 2 (throw 5))) with (λ x x)))
          (term 5))
    (test red
          (term (catch (+ 1 (+ 2 (+ 4 5))) with (λ x 222)))
          (term 12))
    (test red
          (term (add1 (throw 1)))
          (term (throw 1)))
    ; Fails in handler-iswim but succeeds in CC version
    (test red
          (term (catch (+ 1 (+ 2 (catch (+ 1 (throw 44)) with (λ x x)))) with (λ x 222)))
          (term 47))
    ; Exception handler simply throws exception to next level
    (test red
          (term (catch (+ 1 (+ 2 (catch (+ 1 (throw 44)) with (λ x (throw 100))))) with (λ x 222)))
          (term 222))
    ; Tests of operator failures
    (test red
          (term (/ 8 2))
          (term 4))
    (test red
          (term (/ 8 0))
          (term (throw 8)))
    (test red
          (term (catch (/ 2 0) with (λ x x)))
          (term 2))
    (test red
          (term (5 3))
          (term (throw 5)))
    (test red
          (term (x 3))
          (term (throw 0)))
    (test-results)))