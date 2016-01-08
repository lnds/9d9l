(defproject toque-y-fama "0.1.0-SNAPSHOT"
  :description "Toque y Fama"
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :main ^:skip-aot toque-y-fama.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
