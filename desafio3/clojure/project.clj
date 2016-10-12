(defproject ordenar-vector "0.1.0-SNAPSHOT"
  :description "ordenar/vector"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot ordenar-vector.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
