(use 'hopp.gramm)
(use 'hopp.max)


(def G
  '(
    (S  -> ((S1 a)))
    (S1 -> ((a S1 b a a b)(a b a a b)))
    ))



(def tg  (build-tagged-grammar G '(S S1)))
(def sys (tagged-grammar-to-system tg 'S 9 15))


(check-system sys)

;; notOP: L = a^n b^n U c^n (ba)^n

(def sys1 (ex-to-sys '([a :< a :< a :. b :> b :> b]
                       [a :. b]
                       [c :< c :< c :. b :. a :> b :. a :> b :. a]
                       [c :. b :. a])
                     5))

(check-system sys1)

(def sys2 (ex-to-sys '([a :< a :< a :. b :> b :> b :> a]
                       [a :. b :> a]
                       [c :< c :< c :. b :. a :> b :. a :> b :. a :> a]
                       [c :. b :. a :> a])
                     7))

(check-system sys2)
