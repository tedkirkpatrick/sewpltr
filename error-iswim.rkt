#lang racket
(current-load-relative-directory "/Users/ted/Documents/Research/Scheme")
(require "compiled/chapter-16_rkt.zo")

(define-extended-language error-iswim
  iswim
  (M .... (err b))
  ((V U W) .... (err b)))

(define-metafunction/extension δ error-iswim
  [(δerr (o b_1 ... (λ X N) V_3 ... V_4)) (err 1)])