;; Hoppo, a HOP tool
;; MPradella MMXVII
;; ----------------
;; HOP Grammar tools
;; ----------------
;; Example test @repl:
;; (use 'hopp.gramm :reload)(-main "examples/ex1.clj")

(ns hopp.gramm)
(require 'clojure.set)
(use 'hopp.utils)

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

(defn compute-tags
  "computes all the tagged k-words it can find in 'steps' derivations of G"
  [G axiom k steps]
  (let [bord (border k)]
    (loop [sfs  (list (into bord (concat (list :< (->Nonterm axiom) :>) bord)))
           tags #{ (into [:# :.] bord) } ; the starting tagged word
           cnt  0]
      (if (or (empty? sfs)(== steps cnt))
        (do
          (println "Found tagged k-words:")
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

