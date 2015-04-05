(ns qutils.vector
  (:refer-clojure :exclude [rand])
  (:require [qutils.math :refer [sq]]
            [qutils.util :refer [rand]]))

(defn same-dimensions?
  [p q]
  (= (count p) (count q)))

(defn scale
  [scalar point]
  (mapv (partial * scalar) point))

(defn sum
  ([p q]
   {:pre [(same-dimensions? p q)]}
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
  {:pre [(same-dimensions? low-corner high-corner)]}
  (vec (for [[floor ceiling] (map vector low-corner high-corner)]
         (rand floor ceiling))))

(defn vec-comp
  "Given a binary predicate `op` and two vectors `p` and `q`, return true if
  `op` is truthy for every corresponding pair of components between `p` and `q`,
  false otherwise."
  [op p q]
  {:pre [(same-dimensions? p q)]}
  (loop [pq (map vector p q)]
    (if-let [[p_i q_i] (first pq)]
      (if (op p_i q_i)
        (recur (rest pq))
        false)
      true)))

(defn vec<
  "Given two vectors `p` and `q`, return true if every component of `p` is less
  than the corresponding component of `q`, false otherwise."
  [p q]
  (vec-comp < p q))

(defn vec<=
  "Given two vectors `p` and `q`, return true if every component of `p` is less
  than or equal to the corresponding component of `q`, false otherwise."
  [p q]
  (vec-comp <= p q))

(defn vec>
  "Given two vectors `p` and `q`, return true if every component of `p` is
  greater than the corresponding component of `q`, false otherwise."
  [p q]
  (vec-comp > p q))

(defn vec>=
  "Given two vectors `p` and `q`, return true if every component of `p` is
  greater than or equal to the corresponding component of `q`, false otherwise."
  [p q]
  (vec-comp >= p q))

(defn dist-sq
  [p q]
  (let [diffs (map - p q)]
    (apply + (map sq diffs))))

(defn dist
  [p q]
  (Math/sqrt (dist-sq p q)))
