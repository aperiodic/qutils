(ns qutils.vector)

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
