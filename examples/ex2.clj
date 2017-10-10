(use 'hopp.max)


;; notOP: L = a^n b^n U c^n (ba)^n

(def sys1 (ex-to-sys '([a :< a :< a :. b :> b :> b]
                       [a :. b]
                       [c :< c :< c :. b :. a :> b :. a :> b :. a]
                       [c :. b :. a])
                     5))

(reduction-star '[c c c c b a b a b a b a] sys1)
(write-automaton (automaton-transitions (automaton-states sys1)))

