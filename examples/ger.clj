(use 'hopp.gramm)
(use 'hopp.max)


(def G1
  '((S -> ((a b C)(a S b C)))
    (C -> ((a b)))
    ))


(def tg  (build-tagged-grammar G1 '(S C)))
(def sys (tagged-grammar-to-system tg 'S 3 25))
(check-system sys)


(def G2
  '((S -> ((a b a a b)(a S b a a b)))
    ))


(def tg2  (build-tagged-grammar G2 '(S)))
(def sys2 (tagged-grammar-to-system tg2 'S 7 25))
(check-system sys2)


(def G3
  '((S -> ((a b a a a b)(a S b a a a b)))
    ))


(def tg3  (build-tagged-grammar G3 '(S)))
(def sys3 (tagged-grammar-to-system tg3 'S 9 25))
(check-system sys3)

(def G4
  '((S -> ((a b a a a a b)(a S b a a a a b)))
    ))


(def tg4  (build-tagged-grammar G4 '(S)))
(def sys4 (tagged-grammar-to-system tg4 'S 11 25))
(check-system sys4)
