(ns qutils.util)

(defn abs
  [x]
  (max x (* -1 x)))

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
