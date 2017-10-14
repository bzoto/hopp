
(ns hopp.core
  (:gen-class))

(use 'clojure.java.io)

(defn -main
  [& args]
  (println "--- HOPP alpha0 ---")
  (if (and (not (empty? args))
           (.exists (as-file (first args))))
    (load-file (first args))
    (do
      (print "Error: I need a file to work, and ")
      (print args) 
      (println " is not a valid filename")))
  (shutdown-agents))
