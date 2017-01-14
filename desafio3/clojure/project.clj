(defproject ordenar-vector "0.1.0-SNAPSHOT"
  :description "ordenar/vector"
  :dependencies [[org.clojure/clojure "1.8.0"] 
  				 [info.hoetzel/clj-nio2 "0.1.1"]]
  :main ^:skip-aot ordenar-vector.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
