(defproject weather "0.1.0-SNAPSHOT"
  :description "weather - concurrent weather reader"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot weather.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
