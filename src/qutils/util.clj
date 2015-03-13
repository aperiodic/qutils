(ns qutils.util
  (:refer-clojure :exclude [rand rand-int]))

(def alphabet
  (map char (range (-> \a int)
                   (inc (-> \z int)))))

(defn abs
  [x]
  (max x (* -1 x)))

(defn rand
  ([] (rand 0 1))
  ([lo] (rand 0 lo))
  ([lo hi]
   (let [spread (-> (- hi lo) abs)]
     (+ lo (clojure.core/rand spread)))))

(defn rand-int
  ([hi] (rand-int 0 hi))
  ([lo hi]
   (let [spread (-> (- hi lo) abs)]
     (+ lo (clojure.core/rand-int spread)))))

(defn binomial
  [n k]
  (int (apply * (for [i (range 1 (inc k))]
                  (/ (- n (- k i)) i)))))

(defn mostly=
  "Returns true if `x` and `y` differ by less than a billionth."
  [x y]
  (< (abs (- x y)) 1e-9))

(defn interpolate
  "Linearly interpolate `x` to be between zero and one based on its value relative to `lo` and `hi`.
  If `x` is less than or equal to `lo`, the return value will be zero; if `x` is greater than or equal
  to `hi`, the return value will be one."
  [x lo hi]
  (cond
    (<= x lo) 0
    (>= x hi) 1
    :else (/ (- x lo 0.0) hi))) ; the 0.0 is to typecast to a double
