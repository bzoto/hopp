;; Hoppo, a HOP tool
;; MPradella MMXVII
;; ----------------
;; HOP Grammar tools
;; -----------------
;; stuff for working with max-languages
;; (use 'hopp.max :reload)

(ns hopp.max)
(require 'clojure.set)
(use 'hopp.utils)
(use 'clojure.java.shell)

(defrecord Sys [factors k]) ; a "system": i.e. a set of tagged k-words, and k.


(defn is-compatible? ;; erat check_factors
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
    (if (empty? conf)
      (println "No conflicts.")
      (do
        (println "Found conflicts: ")
        (doseq [[x y] conf]
          (pretty-print x)
          (print " vs ")
          (pretty-print y)
          (println))))
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


;; Symmetrical Automaton construction

(defn automaton-states
  [sys]
  (let [factors (:factors sys)
        k       (:k sys)
        states (apply clojure.set/union 
                      (for [x factors
                            y factors
                            :let [flag (and (not= :# (first y))
                                            (not= :# (last x)))
                                  v    (into x (rest y))
                                  r1   (if (and flag (is-compatible? v sys))
                                         (list (into [] (rest v)))
                                         (list))
                                  v1   (into [] (concat (vector-droplast x) y))
                                  r2   (if (and flag (is-compatible? v1 sys))
                                         (cons (vector-droplast v1) r1)
                                         r1)
                                  ]]
                        (into #{} r2)))
        start  (into #{} (for [x (filter #(= :# (first %)) states)]
                            (into [:.] (vector-droplast x))))
        end    (into #{} (for [x (filter #(= :# (last %)) states)]
                            (into [] (concat (rest x) [:.]))))
        ]

    (clojure.set/union states start end)
    ))

(defn transition?
  [state1 state2]
  (= (rest state1)
     (vector-droplast state2)))

(defn automaton-transitions
  [states]
  (into #{}
        (for [x states
              y states
              :when (transition? x y)]
          [x y])))
      
(defn show-automaton
  [transitions]
  (println "Writing the automaton...")
  (let [outfile (clojure.java.io/writer "automa.dot")]
    (binding [*out* outfile]
      (println "digraph finite_state_machine {")
      (println "rankdir = LR")
      (doseq [[from to] transitions]
        (let [l   (int (/ (count from) 2))
              lab (subvec from l (inc l))
              f1  (subvec from 0 l)
              f2  (subvec from l (* 2 l))
              t1  (subvec to 0 l)
              t2  (subvec to l (* 2 l))]
          (print "   \"")(pretty-print f1)
          (print ", ")(pretty-print f2)
          (print "\" -> \"")(pretty-print t1)
          (print ", ")(pretty-print t2)
          (print "\" [label = \"")(pretty-print lab)(print "\"")
          (println "] ")))
      (println "}"))
    (.close outfile))
  (clojure.java.shell/sh "dot"  "automa.dot" "-Tpdf" "-oautoma.pdf"))
      
