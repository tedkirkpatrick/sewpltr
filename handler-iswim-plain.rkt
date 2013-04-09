#lang racket
;(require "compiled/iswim_rkt.zo")
(require "iswim.rkt")
(require "handler-red-test.rkt")

(define-extended-language handler-iswim
  iswim
  (M .... (throw b) (catch M_1 with (λ X M_2)))
  (F hole (V F) (F M) (o V ... F M ...))
  (E .... (catch E with (λ X M)))
  )

(define-metafunction/extension subst iswim
  [(subst-handler (catch M_1 with (λ X_2 M_2)) X_3 M_3)
   (catch (subst-handler M_1 X_3 M_3) with (subst-handler (λ X_2 M_2) X_3 M_3))]
  )

(define handler-red
  (extend-reduction-relation
   iswim-red
   handler-iswim
   ; Override βv rule in iswim
   (--> (in-hole F ((λ X M) V))
        (in-hole F (subst-handler M X V))
        βv)
   (--> (in-hole F (catch (throw b) with (λ X M)))
        (in-hole F ((λ X M) b))
        catch)
   (--> (in-hole F (catch V with (λ X M)))
        (in-hole F V)
        return)
   (--> (in-hole E (catch (in-hole F (throw b)) with (λ X M)))
        (in-hole E (catch (throw b) with (λ X M)))
        caught
        (side-condition
         (not (equal? (term hole) (term F)))))
   (--> (in-hole F (throw b))
        (throw b)
        throw
        (side-condition
         (not (equal? (term hole) (term F)))))
   ))
   
(define test-it (lambda () (handler-red-test handler-red)))
;(current-traced-metafunctions 'all)