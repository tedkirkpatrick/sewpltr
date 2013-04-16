#lang racket

(provide set-empty set-singleton set-list set-list-except set-union
         set-diff set-lookup set-cardinality set-to-list)

(define (set-empty) (make-immutable-hash))

(define (set-singleton var) (make-immutable-hash (list (cons var #t))))

(define (set-list lvar)
  (make-immutable-hash (map (lambda (v) (cons v #t)) lvar)))

(define (set-list-except lvar except)
  (set-list (filter (lambda (v) (not (equal? v except))) lvar)))

(define (set-union lsets)
  (letrec [(add-list (lambda (set lv)
                    (if (null? lv)
                        set
                        (add-list 
                         (hash-set set (car lv) #t)
                         (cdr lv)))))
        (union-two (lambda (a b)
                     (add-list a (hash-keys b))))]
    (foldl union-two (make-hash) lsets)))

(define (set-diff a b)
  (let [(deleted 
         (foldl (lambda (del set) (hash-set set del #f)) a (hash-keys b)))]
    (set-list (filter (lambda (n) (hash-ref deleted n)) (hash-keys deleted)))))

(define (set-lookup val set)
  (hash-ref set val #f))

(define (set-cardinality set)
  (hash-count set))

(define (set-to-list s)
  (hash-keys s))
