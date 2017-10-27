(use 'hopp.gramm)
(use 'hopp.max)


(def G1
  '(
    (A -> ((T end A)(S end A)(L end A)(T end)(S end)(L end)))
    (T -> ((id is id)))
    (L -> ((int is S)))
    ;(L -> ((int is id := E)(int is if E then S)(int is while E do S))) ;; this is k=3
    (S -> ((id := E)(if E then S)(while E do S)))
    (E -> ((id + id)(id)(<< E >>)))
    ))


(def tg  (build-tagged-grammar G1 '(T L S E A)))
(def sys (tagged-grammar-to-system tg 'A 5 35))
(check-system sys)
