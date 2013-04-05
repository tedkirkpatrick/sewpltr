#lang racket
(require redex)
(provide iswim-red-test)

(define (iswim-red-test red)
  (test-->> red
            (term (add1 1))
            (term 2))
  (test-->> red
            (term (/ 8 2))
            (term 4))
  (test-->> red
            (term (+ 2 3))
            (term 5))
  (test-->> red
            (term (+ (λ x x) 2))
            (term (err 1)))
  (test-->> red
            (term (+ 1 (λ x y)))
            (term (err 1)))
  (test-->> red
            (term (/ 8 0))
            (term (err 0)))
  (test-->> red
            (term (add1 (λ x x))) 
            (term (err 1)))
  )