(ns qutils.curve-test
  (:require [clojure.test :refer :all]
            [qutils.curve :refer :all]))

(deftest linear-test
  (let [id (linear 0 1)]
    (doseq [i (range 0 1 0.001)]
      (is (= i (id i) (position-at id i))))))
