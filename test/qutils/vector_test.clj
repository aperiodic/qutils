(ns qutils.vector-test
  (:refer-clojure :exclude [rand-int])
  (:require [clojure.test :refer :all]
            [qutils.util :refer [rand-int]]
            [qutils.vector :refer :all]))

(deftest scale-test
  (is (= [2 4 6] (scale 2 [1 2 3])))
  (dotimes [_ 1e5]
    (let [coefficient (rand 1e4)]
      (is (= (map (partial * coefficient) (range 10))
             (scale coefficient (vec (range 10))))))))

(deftest sum-test
  (is (= [3 5 7] (sum [1 2 3] [2 3 4])))
  (is (= [6 9 12] (sum [1 2 3] [2 3 4] [3 4 5])))
  (dotimes [_ 1e5]
    (let [n (rand-int 1 10)
          v1 (vec (repeatedly n #(rand 1e4)))
          v2 (vec (repeatedly n #(rand 1e4)))]
      (is (= (mapv + v1 v2) (sum v1 v2))))))

(deftest difference-test
  (is (= [1 1 1] (difference [2 3 4] [1 2 3])))
  (is (= [1 0 -1] (difference [2 3 4] [1 2 3] [0 1 2]))))
