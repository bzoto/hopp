;; at the repl:
;; (-main "examples/ex1.clj")

(use 'hopp.max)
(use 'hopp.utils)
(require 'clojure.set)


(def init #{  
            '[:# :. :#]
            '[:# :< a]
            '[b  :> :#]
            '[a  :< a]
            })

(def possible-factors
  (for [
        x '(a b)
        y '(a b)
        ]
    (for [p '(:> :< :.)]
      [x p y])))


(def systems
  (filter nonconflictual-sys?
          (map #(->Sys (clojure.set/union init %) 3)
               (map set
                    (iterated-p-car possible-factors)))))

(def check1
  (filter #(reduction-star '[a a b a a b b a a b] %) systems))

(println (empty? check1))

(def check2
  (filter #(reduction-star '[a b a a b] %) systems))


(println (empty? check2))
