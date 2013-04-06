#lang racket
(require "compiled/chapter-16_rkt.zo")
(require "error-iswim-test.rkt")

(define-extended-language error-iswim
  iswim
  (M .... (err b))
  (o2 .... /)
  )

(define-metafunction/extension δ error-iswim
  [(δerr (/ b_1 0)) (err 0)]
  [(δerr (/ b_1 b_2)) ,(/ (term b_1) (term b_2))]
  )

(define error-red
  (extend-reduction-relation
   iswim-red
   error-iswim
   (--> (in-hole E (o b ...))
        (in-hole E (δerr (o b ...)))
        δ)
   (--> (in-hole E (o b ... (λ X M) V ...))
        (err 1)
        δerr)
   (--> (in-hole E (b V))
        (in-hole E (err b))
        δerr2)
   (--> (in-hole E (err b))
        (err b)
        err
        (side-condition 
         (not (equal? (term hole) (term E)))))
   ))

;(current-traced-metafunctions 'all)