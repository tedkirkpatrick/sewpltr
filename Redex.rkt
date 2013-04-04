#lang racket
(require redex)
(define-language bool-any-lang
  [B true
     false
     (or B B)]
  [C (or C B)
     (or B C)
     hole])