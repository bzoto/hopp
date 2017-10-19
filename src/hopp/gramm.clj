;; Hoppo, a HOP tool
;; MPradella MMXVII
;; ----------------
;; HOP Grammar tools
;; ----------------
;; Example test @repl:
;; (use 'hopp.gramm :reload)
;; (load-file "examples/ex1.clj")

(ns hopp.gramm)
(require 'clojure.set)
(use 'hopp.utils)
(use 'hopp.max)

(defrecord Nonterm [nonterm])
(defn Nonterm? [x] (instance? Nonterm x))

(defn drop-nt [lst]
  (filter #(not (Nonterm? %)) lst))


(defn build-tagged-grammar
  "takes a grammar as a list of lists (e.g. ((A -> ((a A) (a))) ...); 
  it returns a hash table of the rules (with right parts parenthesized with :< :> pairs).
  If excluded-nt is present, it is a list of nonterminals used for simulating EBNF,
  thus their associated right parts are not parenthesized."
  [gr nt & excluded-nt]
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
                        (let [right-part (map (fn [u]
                                                (if (some #{u} nt)
                                                  (->Nonterm u)
                                                  u))
                                              t)]
                          (if (some #{(first rule)} (first excluded-nt))
                            (into [] right-part)
                            (into [:<] (concat right-part [:>])))))
                      (nth rule 2))))))))

(defn terminal? [x]
  (not (or
         (Nonterm? x)
         (contains? #{:< :> :.} x))))


(defn terminal-sf?
  "is it a terminal sentential form?"
  [sf]
  (not-any? Nonterm? sf))


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

(defn fix-tags
  [sf]
  (let [sf1 (filter #(not (Nonterm? %)) sf)] ; drop nonterminals
    (loop [cur   (first sf1)
           xs    (rest sf1)
           out   []
           drop> false
           ]
      (if (empty? xs)
        (conj out cur)
        (let [nxt (first xs)]
          (cond
            (and drop> (= cur :>)(not= nxt :>)) (recur nxt (rest xs)
                                                       (if (and (= :# (last out))
                                                                (= :# nxt))
                                                         (conj out :.)
                                                         (conj out :>))
                                                       false)
            
            (or
             (and drop> (= cur :>)) ; this is for copy rules  
             (and (= cur :<)(= nxt :<)) 
             (and (= cur :>)(= nxt :>))) (recur nxt (rest xs) out drop>)
            
            ;; copy rules
            (and (= cur :<)(= nxt :>)) (recur nxt (rest xs) out true)

            (or (and (= :# cur)(= :# nxt))
                (and (terminal? cur)(terminal? nxt))) (recur nxt (rest xs) (-> out (conj cur)(conj :.)) drop>)
            
            :else (recur nxt (rest xs) (conj out cur) drop>)))))))

(defn tagged-grammar-to-system
  "computes all the tagged k-words it can find in 'steps' derivations of G;
  it returns the resulting Red system"
  [G axiom k steps]
  (if-not (and (>= k 3)
               (odd? k))
    (do
      (print "Bad k: ")(println k)
      nil)

    (let [bord (untagged-border (+ k 2))]
      (loop [sfs  (list (into bord (concat (list (->Nonterm axiom)) bord)))
             tags #{}
             cnt  0]
        (if (or (empty? sfs)(== steps cnt))
          (do
            (println "Found tagged k-words:")
            (doseq [t tags]
              (pretty-print t)
              (println))
            (println "-----------")
            (->Sys tags k))
          (let [y  (apply-rules (first sfs) G)
                x  (filter (fn [t] (not (terminal-sf? t))) y)
                xs (rest sfs)]
            (recur
              (concat xs x)
              (reduce clojure.set/union tags (map #(k-factors (fix-tags %) k) y))
              (inc cnt))))))))
