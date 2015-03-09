(ns qutils.curve)

(defprotocol Curve
  "The protocol for curves used in animations. In order to implement a new kind of animation curve,
  simply implement this protocol."

  (position-at [curve t]
    "Return the position of the curve at time `t`. The `t` argument will be clamped to remain
    between 0 and 1, inclusive."))


(deftype Linear
  [lo hi]
  Curve
  (position-at [curve t]
    (case t
      (0 0.0) lo
      (1 1.0) hi
      (let [diff (- hi lo)]
        (+ lo (* t diff)))))

  clojure.lang.IFn
  (invoke [curve t] (position-at curve t))
  (applyTo [curve args] (clojure.lang.AFn/applyToHelper curve args)))

(defn linear
  [lo hi]
  (Linear. lo hi))
