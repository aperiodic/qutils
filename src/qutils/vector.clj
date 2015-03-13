(ns qutils.vector
  (:refer-clojure :exclude [rand])
  (:require [qutils.util :refer [rand]]))

(defn scale
  [scalar point]
  (mapv (partial * scalar) point))

(defn sum
  ([p q]
   {:pre [(= (count p) (count q))]}
   (mapv + p q))
  ([p q & rs]
   (loop [summed (sum p q), remaining rs]
     (if-let [r (first remaining)]
       (recur (sum summed r) (rest remaining))
       summed))))

(defn difference
  ([p q]
   (sum p (scale -1 q)))
  ([p q & rs]
   (loop [diff (difference p q), remaining rs]
     (if-let [r (first remaining)]
       (recur (difference diff r) (rest remaining))
       diff))))

(defn rand-point
  "Given two points, return a random point within the n-box that those two points define. The points
  must have the same number of dimensions."
  [low-corner high-corner]
  {:pre [(= (count low-corner) (count high-corner))]}
  (vec (for [[floor ceiling] (map vector low-corner high-corner)]
         (rand floor ceiling))))
