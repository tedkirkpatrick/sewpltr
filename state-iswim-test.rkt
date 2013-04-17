#lang racket
(require redex)

(provide run-state-tests)

(define (run-state-tests test)
  (test (term ((λ x (set x 2)) 5)) 5)
  ; Sequencing using primitives
  (test (term (((λ y1 (λ y2 y2)) ((λ x (set x 3)) 1)) ((λ y x) 2))) 3)
  ; Equivalent to above, using seq
  (test (term (seq ((λ x (set x 3)) 1) x)) 3)
  ; Equivalent to above, adding let
  (test (term (seq (let ((x = 1)) in (set x 3)) x)) 3)

  ; Testing let
  (test (term (let ((x = 1) (y = 2)) in (+ x y))) 3)
  (test (term (let () in (+ 1 1))) 2)
  ; Note: Unspecified behaviour for repeated variables in let
  
  ; Testing seq, assuming let works
  (test (term (seq (let ((x = 1) (y = 2)) in (seq (set y 4) (set x 5))) (+ x y))) 9)
  (test (term (seq (let ((x = 1) (y = 2)) in (seq (set y 4) (set x 5) (set y 12))) (+ x y))) 17)
  
  (test (term (+ (let ((x = 1)) in (set x 2)) x)) 3)
  
  ; Arguments evaluated in left to right order
  (test (term (let ((x = 1)) in (+ (set x 2) x))) 3)
  (test-results))