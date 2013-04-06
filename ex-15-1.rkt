#lang racket
;(current-load-relative-directory "/Users/ted/Documents/Research/Scheme")
(require "compiled/chapter-16_rkt.zo")

(define Y  (term (λ f (λ x (f (λ v ((x x) v)))) (λ x (f (λ v ((x x) v)))))))
