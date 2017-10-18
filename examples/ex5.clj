(use 'hopp.gramm)
(use 'hopp.max)


(def G1
  '((S -> ((X)(Y)))
    (Y -> ((0 a a Y)(c)))
    (X -> ((a X b a a b)(a b a a b)))
    ))


(def tg  (build-tagged-grammar G1 '(S X Y)))
(def sys (tagged-grammar-to-system tg 'S 7 15))


(check-system sys)

(reduction-star '[a a b a a b b a a b] sys)
(reduction-star '[0 a a 0 a a c] sys)

(def trans (automaton-transitions (automaton-states sys)))
(write-automaton trans)
