;; at the repl:
;; (-main "examples/ex1.clj")

(use 'hopp.gramm)
(use 'hopp.max)

(def G
  '((S -> ((a S b a a b)(a b a a b)))))


(def tg  (build-tagged-grammar G '(S)))
(def sys (tagged-grammar-to-system tg 'S 7 25))


(check-system sys)

(factor-precs '(a a b a) sys)

(reduction-star '[a a a b a a b b a a b b a a b] sys)
(reduction-star '[a a a a a b a a b b a a b b a a b] sys)
(reduction-star '[a a a b a a b b a a b b a a b b a a b] sys)

(def trans (automaton-transitions (automaton-states sys)))
(write-automaton trans)


