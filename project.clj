(defproject hopp "HOPP"
  :description "A tool for Higher-order Operator Precedence languages"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 ]
  :aot  [hopp.core]
  :main hopp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
