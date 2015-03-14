(ns qutils.animation
  (:require [qutils.curve :as curve]
            [qutils.util :refer [interpolate]]))

(defn- kw->quantity
  "Turn any keyword or symbol that starts with numbers and ends with letters into a two vector of
  the quantity (the number) and units (the letters). If not given a such a value, returns a vector
  with that value as the first argument and the empty keyword as the second."
  [x]
  (let [digits-then-letters? (fn [named] (re-matches #"(\d+)([a-z]+)" (name named)))]
  (cond
    (and (not (keyword? x)) (not (symbol? x)))
    [x (keyword "")]

    (not (digits-then-letters? x))
    [x (keyword "")]

    :else
    (let [[_ q u] (digits-then-letters? x)]
      [(Integer/parseInt q) (keyword u)]))))

(defrecord Animation
  [curve start stop]
  clojure.lang.IFn
  (invoke [_ time]
    (let [t (interpolate time start stop)]
      (curve t)))
  (applyTo [anim args] (clojure.lang.AFn/applyToHelper anim args)))

(defn animation
  "Create an animation using the supplied curve, that starts at the `start` time and has the
  specified `duration`.  The `start` and `duration` are used to derive `t` for the curve when the
  animation is evaluated

  The `start` and `duration` arguments may be either regular clojure numbers, or keywords that
  describe an integer with units, such as :10s or :1m. If these two arguments have different units,
  and neither unit is the empty unit, then an error is thrown."
  [curve start duration]
  (when-not (satisfies? curve/Curve curve)
    (throw (IllegalArgumentException.
             "the animation's curve must implement the qutils.curve/Curve protocol.")))

  (let [empty-kw (keyword "")
        [start start-units] (kw->quantity start)
        [duration duration-units] (kw->quantity duration)]
    (when (and (not= start-units duration-units)
               (not= start-units empty-kw)
               (not= duration-units empty-kw))
      (throw (IllegalArgumentException. "mismatched units")))
    (Animation. curve start (+ start duration))))
