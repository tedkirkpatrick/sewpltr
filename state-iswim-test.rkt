#lang racket
(require redex)

(provide run-state-tests)

(define (run-state-tests test [run-closures #t])
  (test (term ((λ x (set x 2)) 5)) 5)
  ; Sequencing using primitives. Next two steps refine to let ... seq ... form.
  (test (term ((λ x (((λ y1 (λ y2 y2)) (set x 3)) x)) 1)) 3)
  ; Equivalent to above, using seq
  (test (term ((λ x (seq (set x 3) x)) 1)) 3)
  ; Equivalent to above, adding let
  (test (term (let ((x = 1)) in [seq (set x 3) x])) 3)

  ; Testing let
  (test (term (let ((x = 1) (y = 2)) in (+ x y))) 3)
  (test (term (let () in (+ 1 1))) 2)
  ; Note: Unspecified behaviour for repeated variables in let
  
  ; Testing seq, assuming let works
  (test (term (let ((x = 1) (y = 2)) in (seq (set y 4) (set x 5) (+ x y)))) 9)
  (test (term (let ((x = 1) (y = 2)) in (seq (set y 4) (set x 5) (set y 12) (+ x y)))) 17)
  
  (test (term (+ (let ((x = 1)) in (set x 2)) 2)) 3)
  
  ; Arguments evaluated in left to right order
  (test (term (let ((x = 1)) in (+ (set x 2) x))) 3)
  
  ; Complex nested expression (but all variables used within lexical scope)
  (test (term (let ((out = 1)) in (seq (let ((inner = 2)) in (set out (add1 inner))) (add1 out)))) 4)

  ; Variable location retained in a closure and used out of lexical scope
  (when run-closures (test (term (let ((out = 1)) in (seq (let ((inner = 2)) in (set out (λ x (+ x inner)))) (out 5)))) 7))
  (test-results))