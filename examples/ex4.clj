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
(def sys (tagged-grammar-to-system tg 'S 3 65))
(check-system sys)

(reduction-star '[a a a c b b b b c b c b b b b b] sys)

(def trans (automaton-transitions (automaton-states sys)))
;(write-automaton trans)

(def G1
  '(
    (S0 -> ((S a)))
    (S  -> ((B a S c)(B a c)))
    (B  -> ((B b)(b)))
    ))


(def tg1  (build-tagged-grammar G1 '(S0 S B) '(B)))
(def sys1 (tagged-grammar-to-system tg1 'S0 3 65))
(check-system sys1)


(reduction-star '[b b b b a b b a b a c c c a] sys1)
