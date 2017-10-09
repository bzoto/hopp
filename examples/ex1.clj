(use 'hopp.gramm)


(def G
  '((S -> ((a S b a a b)(a b a a b)))))


(def g (build-tagged-grammar G '(S)))
(def sys (->Sys (compute-tags g 'S 7 15) 7))


(check-system sys)

(factor-precs '(a a b a) sys)



(reduction-star '[a a a b a a b b a a b b a a b] sys)
(reduction-star '[a a a a a b a a b b a a b b a a b] sys)
