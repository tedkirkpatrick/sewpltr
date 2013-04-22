#lang racket
(provide store-make store-lookup store-update! store-delete! store-size store-vars)

(define (store-make)
  (make-hash))

(define (store-lookup id st)
  (hash-ref st id #f))

(define (store-update! id val st)
  (hash-set! st id val))

(define (store-delete! id st)
  (if (store-lookup id st)
      (hash-remove! st id)
      (raise-argument-error 'store-delete! "id" id)))

(define (store-size st)
  (hash-count st))

(define (store-vars st)
  (hash-keys st))
