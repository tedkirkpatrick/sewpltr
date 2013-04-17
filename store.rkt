#lang racket
(provide store-make store-lookup store-update! store-size store-vars)

(define (store-make)
  (make-hash))

(define (store-lookup id st)
  (hash-ref st id #f))

(define (store-update! id val st)
  (hash-set! st id val))

(define (store-size st)
  (hash-count st))

(define (store-vars st)
  (hash-keys st))
