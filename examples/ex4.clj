(use 'hopp.gramm)
(use 'hopp.max)


;; L = a^n (c b+)^n
;; example with EBNF simulated through a linear sub-grammar

(def G
  '(
    (S -> ((a S c B)(a c B)))
    (B -> ((b B)(b))) ;; regular part
    ))


(def tg  (build-tagged-grammar G '(S B) '(B))) ;; B does not participate into the structure
(def sys (tagged-grammar-to-system tg 'S 5 65))
(check-system sys)

(reduction-star '[a a a c b b b b c b c b b b b b] sys)
