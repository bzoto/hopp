;; Hoppo, a HOP tool
;; MPradella MMXVII
;; ----------------
;; HOP Grammar tools
;; -----------------
;; basic utils

(ns hopp.utils)
(require 'clojure.set)

(defn k-factors
  "returns all the k-factors (even) of a vector in a set"
  [sf k]
  (loop [i 0
         res #{}
         ]
    (if (< (- (count sf) i) k)
      res
      (recur (+ 2 i)
             (conj res (subvec sf i (+ i k)))))))

(defn border [k]
  "returns the border of order k, k must be odd"
  (if (<= k 3)
    [:#]
    (into [:# :.] (border (- k 2)))))

(defn untagged-border [k]
  "returns the border of order k, no tags (k must be odd > 2)"
  (if (<= k 3)
    [:#]
    (into [:#] (untagged-border (- k 2)))))


(defn pretty-print
  "displays a tagged word in a human-friendly form"
  [sf]
  (doseq [x sf]
    (print (case x
             :< "["
             :> "]"
             :# "#"
             :. "."
             x))))

(defn sigma
  "discards all tags from sf"
  [sf]
  (into [] (filter #(not (contains? #{:< :> :.} %)) sf)))

(defn vector-droplast
  [vec]
  (subvec vec 0 (dec (count vec))))


(defn iterated-p-car
  [list-of-lists]
  (reduce (fn [X Y]
            (for [x X
                  y Y]
              (cond
                (and (list? x)(seq? y)) (concat y x)
                (seq? y) (concat y (list x))
                (seq? x) (concat (list y) x)
                :else (concat (list y)(list x)))))
          list-of-lists))



