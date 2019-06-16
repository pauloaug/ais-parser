(ns ais.core-test
  (:require [clojure.test :refer :all]
            [ais.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))))

(deftest dois-mais-dois (is (= 4 (+ 2 2))))

(run-tests)
