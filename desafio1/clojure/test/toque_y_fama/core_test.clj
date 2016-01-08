(ns toque-y-fama.core-test
  (:require [clojure.test :refer :all]
            [toque-y-fama.core :refer :all]))

(deftest famas-test
  (testing "Valida Famas"
    (is (= (famas [1 2 3] [1 2 3]) 3))
    (is (= (famas [1 2 3] [3 1 2]) 0))))

(deftest toques-test
  (testing "Valida Toques"
    (is (= (toques [1 2 3] [3 1 2]) 3))
    (is (= (toques [1 0 4] [3 1 2]) 1))))
