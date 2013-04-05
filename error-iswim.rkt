#lang racket
(current-load-relative-directory "/Users/ted/Documents/Research/Scheme")
(require "compiled/chapter-16_rkt.zo")

(define-extended-language error-iswim
  iswim
  (o1 .... err)
  (M .... (err b))
  ((V U W) .... (err b)))

(define-metafunction/extension δ error-iswim
  [(δerr (o b_1 ... (λ X N) V_1 ... )) (err 1)]
  [(δerr (err b)) (err b)])

(define error-red
  (extend-reduction-relation
   iswim-red
   error-iswim
   (--> (in-hole E (o b_1 ... (λ X N) V_1 ...))
        (in-hole E (δerr (o b_1 ... (λ X N) V_1 ...)))
        δr-err)
   (--> (in-hole E (err b))
        (err b)
        error)))
                             