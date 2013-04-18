#lang racket
(provide store-make store-lookup store-update! store-size store-vars Store%)

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

(define Store%
  (class object%
    (init)
    (define the-store (store-make))
    (super-new)
    
    (define/public (clear)
      (set! the-store (store-make)))
    
    (define/public (lookup id)
      (store-lookup id the-store))
    
    (define/public (update! id val)
      (store-update! id val the-store))
    
    (define/public (size)
      (store-size the-store))
    
    (define/public (vars)
      (store-vars the-store))
    ))
