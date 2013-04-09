#lang racket
;(require "compiled/iswim_rkt.zo")
(require "iswim.rkt")

(define-extended-language handler-iswim
  iswim
  (M .... (throw b) (catch M_1 with (λ X M_2)))
  (E hole (V E) (E M) (o V ... E M ... ))
  )

(define-metafunction/extension subst iswim
  [(subst-handler (catch M_1 with (λ X_1 M_2)) X_2 M_3)
   (catch (subst-handler M_1 X_2 M_3) with (subst-handler (λ X_1 M_2) X_2 M_3))]
  )

(define handler-red
  (extend-reduction-relation
   iswim-red
   handler-iswim
   ; Override βv rule in iswim
   (--> (in-hole E ((λ X M) V))
        (in-hole E (subst-handler M X V))
        βb)
   (--> (in-hole E (catch (throw b) with (λ X M)))
        (in-hole E ((λ X M) b))
        catch)
   (--> (in-hole E (throw b))
        (throw b)
        throw)
   (--> (in-hole E (catch V with (λ X M)))
        (in-hole E V)
        return)
   ))
   

(current-traced-metafunctions 'all)