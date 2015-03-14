(ns qutils.curve-test
  (:require [clojure.test :refer :all]
            [qutils.curve :refer :all]))

(deftest linear-test
  (let [id (linear 0 1)]
    (doseq [i (range 0 1 0.001)]
      (is (= i (id i) (position-at id i))))))

(deftest bezier-test
  (let [linear-bez (bezier [[0 0] [1 1]])]
    (doseq [i (range 0.0 1.0 0.001)]
      (is (= [i i] (linear-bez i) (apply linear-bez [i])))))

  (let [quad-bez (bezier [[0 0] [1 0] [1 1]])]
    (doseq [i (range 0.0 1.0 0.001)]
      (is (>= i (nth (quad-bez i) 1))))))
