#lang racket
(require test-engine/racket-tests)
(require redex)
(require "set.rkt")

(check-expect 
 (set-lookup (term a) (set-empty))
 #f)

(check-expect 
 (set-cardinality (set-empty))
 0)

(define z (set-singleton (term z)))

(check-expect
 (set-lookup (term z) z)
 #t)

(check-expect
 (set-lookup (term a) z)
 #f)

(define a (set-list (term (a b c d))))

(check-expect 
 (set-lookup (term c) a)
 #t)

(check-expect
 (set-lookup (term q) a)
 #f)

(define q (set-list (term (q))))
(define tt (set-union (list a q z)))

(check-expect
 (set-lookup (term q) tt)
 #t)

(check-expect
 (set-lookup (term a) tt)
 #t)

(check-expect
 (set-lookup (term m) tt)
 #f)

(check-expect
 (set-cardinality tt)
 6)

(check-expect
 (set-cardinality tt)
 (foldl (lambda (a b) (+ a b)) 0 (map set-cardinality (list a q z))))

(check-expect
 (set-diff q (set-empty))
 (set-singleton (term q)))

(check-expect
 (set-cardinality (set-diff q q))
  0)

(check-expect
 (set-diff tt a)
 (set-list (term (q z))))

(test)


    