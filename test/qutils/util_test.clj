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
  (doseq [i (range 0 10 0.01)]
    (is (mostly= (/ i 10) (interpolate i 0 10))))

  (doseq [i (range 15 25 0.01)]
    (is (mostly= (-> i (- 15) (/ 10))
                 (interpolate i 15 25)))))

(deftest restore-state-test
  (let [!a (atom nil)]
    (do (restore-state !a ":value")
      (is (= :value @!a)))))
