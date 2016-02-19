(defproject crdr "0.1.0-SNAPSHOT"
  :description "crdr - concurrent news reader"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clj-tagsoup/clj-tagsoup "0.3.0" :exclusions [org.clojure/clojure]]]
  :main ^:skip-aot crdr.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
