;; Hoppo, a HOP tool
;; MPradella MMXVII
;; ----------------
;; HOP Grammar tools
;; ----------------
;; Example test @repl:
;; (use 'hopp.gramm :reload)(-main "examples/ex1.clj")

(ns hopp.gramm)
(require 'clojure.set)
(require 'clojure.string)
(use 'clojure.test)

(defrecord Nonterm [nonterm])
(defn Nonterm? [x] (instance? Nonterm x))

(defn drop-nt [lst]
  (filter #(not (Nonterm? %)) lst))


(defn put-tags
  "takes a sentential form and inserts tags"
  [sf]
  (loop [i   1
         old (first sf)
         res (list old)]
    (if (= i (count sf))
      (into [:<] (concat res [:>]))
      (let [cur (nth sf i)]
        (recur (inc i)
               cur
               (concat res (if-not (or (Nonterm? cur)
                                       (Nonterm? old))
                             (list :. cur)
                             (list cur))))))))

(defn build-tagged-grammar
  "takes a grammar as a list of lists (e.g. ((A -> ((a A) (a))) ...); 
  it returns a hash table of the rules"
  [gr nt]
  (loop [r gr
         G {}]
    (if (empty? r)
      G
      (let [rule (first r)]
        (recur
         (rest r)
         (assoc G
                (->Nonterm (first rule))
                (map (fn [t]
                       (put-tags
                        (map (fn [u]
                               (if (some #{u} nt)
                                 (->Nonterm u)
                                 u))
                             t)))
                     (nth rule 2))))))))

(defn terminal? [x]
  (not (or
        (Nonterm? x)
        (contains? #{:< :> :.} x))))


(defn terminal-sf?
  "is it a terminal sentential form?"
  [sf]
  (not-any? Nonterm? sf))


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

(defn apply-rules
  "applies all the possible rules in G to the sentential form sf"
  [sf G]
  (loop [out  '()
         left '()
         right sf]
    (if (empty? right)
      (map #(into [] %) out)
      (let [[x & xs]  right]
        (recur (if (Nonterm? x)
                 (into (map #(concat left % xs)
                            (G x))
                       out)
                 out)
               (concat left (list x))
               xs)))))


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

(defn fix-axiom-copy-tag ; the horror, the horror!
  [sf]
  (loop [i 0]
    (if (>= i (- (count sf) 2))
      sf
      (if (= [:< :. :>]
             (subvec sf i (+ i 3)))
        (into
         (subvec sf 0 i)
         (concat [:.]
                 (subvec sf (+ i 3))))
        (recur (inc i))))))
  

(defn fix-tags
  "takes a completely parenthesized and tagged sentential form; 
  it drops non-terminals and redundant parentheses"
  [sf]
  (loop [i 0
         res '()]
    (if (>= i (- (count sf) 2))
      (fix-axiom-copy-tag
       (into [] (concat res (subvec sf i))))
      (let [cur   (nth sf i)
            next  (nth sf (inc i))
            nnext (nth sf (+ 2 i))]
        (cond
          ;; copy rule
          (and (= cur :<)(Nonterm? next)(= nnext :>)) (recur (+ 2 i) (concat res (list :< :.)))
          
          (or
           (and (= cur :>)(= next :>)) 
           (and (= cur :<)(= next :<))
           (and (Nonterm? cur)(= next :>))) (recur (inc i) res)

          (and (= cur :<)(Nonterm? next)) (recur (+ 2 i) (concat res (list :<)))
          
          (and (terminal? cur)(Nonterm? next)(terminal? nnext)) (recur (+ 2 i)
                                                                       (concat res (list cur :.)))
            
          :else (recur (inc i) (concat res (list cur))))))))

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


(defn compute-tags
  "computes all the tagged k-words it can find in 'steps' derivations of G"
  [G axiom k steps]
  (let [bord (border k)]
    (loop [sfs  (list (into bord (concat (list :< (->Nonterm axiom) :>) bord)))
           tags #{ (into [:# :.] bord) }
           cnt  0]
      (if (or (empty? sfs)(== steps cnt))
        (do
          (println "Found tags:")
          (doseq [t tags]
            (pretty-print t)
            (println))
          (println "-----------")
          tags)
        (let [y  (apply-rules (first sfs) G)
              x  (filter (fn [t] (not (terminal-sf? t))) y)
              xs (rest sfs)]
          (recur
           (concat xs x)
           (reduce clojure.set/union tags (map #(k-factors (fix-tags %) k) y))
           (inc cnt)))))))


(defrecord Sys [factors k]) ; a "system": i.e. a set of tagged k-words, and k.

(defn sigma
  "discards all tags from sf"
  [sf]
  (into [] (filter #(not (contains? #{:< :> :.} %)) sf)))

(defn is-compatible? ;; check_factors
  "check if a sf is compatible with a system"
  [sf sys]
  (let [k   (:k sys)
        fac (:factors sys)]
    (clojure.set/subset? (k-factors sf k) fac)))

(defn conflictual?
  [s1 s2]
  (and (not= s1 s2)
       (= (sigma s1)(sigma s2))))


(defn check-system
  "checks if a system is conflictual or not, returning a sequence of conflicts"
  [sys]
  (let [conf 
        (for [x (:factors sys)
              y (:factors sys)
              :when (conflictual? x y)]
          (list x y))]
    (when-not (empty? conf)
      (println "Found conflicts: ")
      (doseq [[x y] conf]
          (pretty-print x)
          (print " vs ")
          (pretty-print y)
          (println)))
    conf))

(defn factor-precs
  "get precedences for a subword. If nil: it is not an allowed factor"
  [factor sys]
  (let [factors (:factors sys)
        tagged  (for [f factors
                      :when (= factor (sigma f))]
                  f)]
    (if (empty? tagged)
      (do 
        (print "*** factor-precs: found a bad factor: ")
        (println factor)
        nil)
      (first tagged))))

(defn insert-precs
  "insert the correct precedences in a vector;
  it assumes a non-conflictual system"
  [vec sys]
  (let [k    (:k sys)
        size (inc (int (/ k 2)))
        tagged-factors (loop [from 0
                              lst  '()
                              nil-flag false]
                         (if (< (count vec) (+ from size))
                           (if nil-flag nil lst)
                           (let [c (subvec vec from (+ from size))
                                 pr (factor-precs c sys)]
                             (recur (inc from)
                                    (concat lst (list pr))
                                    (or nil-flag (nil? pr))))))]
    (if (nil? tagged-factors)
      nil
      (let [res1 (into [] (reduce (fn [y x]
                                    (into y (subvec x 0 2)))
                                  []
                                  tagged-factors))]
        (into (subvec res1 0 (- (count res1) 2))
              (last tagged-factors))))))


(defn find-handle
  [sf]
  (loop [i 0
         start nil]
    (cond
      (>= i (count sf)) nil

      (and (= :> (nth sf i))
           (not (nil? start))) [start (inc i)]

      :else (recur
             (inc i)
             (if (= :< (nth sf i)) i start)))))


(defn reduction
  "apply one reduction step of sys"
  [sf sys]
  (let [factors     (:factors sys)
        k           (:k sys)
        hand        (find-handle sf)]
    (if (nil? hand)
      nil 
      (let [[start end] hand]
        (if-some [hole (nth (insert-precs (into [(nth sf (dec start))]
                                                (sigma (subvec sf end (+ end (- k 2)))))
                                          sys)
                            1)] ; this is the tag replacing the handle
          (into (subvec sf 0 start)
                (concat [hole]
                        (subvec sf end)))
          nil)))))

(defn reduction-star
  "apply all the possible reductions of sys to an input x"
  [x sys]
  (let [factors     (:factors sys)
        k           (:k sys)
        bord        (untagged-border k)
        ]
    (if-some [input (insert-precs (into bord (concat x bord)) sys)]
      (do 
        (print "START: ")
        (pretty-print input)(println)
        (loop [cur input]
          (if-some [red (reduction cur sys)]
            (do
              (print "step:  ")
              (pretty-print red)(println)
              (recur red))
            (do 
              (println "STOP.")))))
      (do
        (print "cannot reduce ")
        (println x)))))
