(ns qutils.util-test
  (:refer-clojure :exclude [rand rand-int])
  (:require [clojure.test :refer :all]
            [qutils.util :refer :all]))

(deftest mostly=-test
  (dotimes [_ 1e5]
    (let [x (- (rand) 0.5)]
      (is (mostly= x (+ x (- (rand 1e-9) (/ 1e-9 2))))))))

(deftest abs-test
  (dotimes [_ 1e5]
    (let [x (- (rand) 0.5)]
      (if (> x 0)
        (= x (abs x))
        (= x (abs (* -1 x)))))))

(deftest interpolate-test
  (doseq [i (range 0 1 0.01)]
    (is (mostly= i (interpolate (* 10 i) 0 10)))))
