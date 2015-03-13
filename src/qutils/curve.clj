(ns qutils.curve
  (:require [clojure.string :as str]
            [qutils.util :refer [binomial]]
            [qutils.vector :as vec]))

(defprotocol Curve
  "The protocol for curves used in animations. In order to implement a new kind of animation curve,
  implement this protocol by using the `defcurve` macro below."

  (position-at [curve t]
    "Return the position of the curve at time `t`. The `t` argument will be clamped to remain
    between 0 and 1, inclusive."))

(defmacro defcurve
  "Define a new curve type that implements both the Curve protocol above, and clojure.lang.IFn
  (using the position-at Curve protocol method).

  The `curve` argument is a symbol naming the curve. A deftype with the title-case version of the
  name will be created (e.g. `Linear`), and a convenient constructor function with the lower-case
  version will also be defined. (e.g. `linear`).

  The `fields` are a vector of symbols defining the fields of the curve. These will be the fields of
  the deftype and the arguments of the constructor function. You'll have access to them in the body
  of the `position-fn`.

  The `position-fn` is the implementation of the `position-at` method of the Curve protocol. It has
  the same signature, and the `fields` of the curve are in-scope in its body.

  Here's an example of defining a linear curve type using this macro:

    (defcurve Linear
      [lo hi]
      (position-at
        [_ t]
        (let [diff (- hi lo)]
          (+ lo (* t diff)))))
  "
  [curve fields position-fn]
  (let [[_ args & body] position-fn
        title-curve (-> (str/capitalize (name curve)) symbol)
        lowercase-curve (-> (str/lower-case (name curve)) symbol)]
    `(do
       (deftype ~title-curve
         ~fields
         Curve
         (~'position-at ~args
           ~@body)

         clojure.lang.IFn
         (~'invoke [curve# t#] (~'position-at curve# t#))
         (~'applyTo [curve# args#] (clojure.lang.AFn/applyToHelper curve# args#)))

       (defn ~lowercase-curve
         ~fields
         (new ~title-curve ~@fields)))))

(defcurve Linear
  [lo hi]
  (position-at
    [_ t]
    (case t
      (0 0.0) lo
      (1 1.0) hi
      (let [diff (- hi lo)]
        (+ lo (* t diff))))))

(defcurve Bezier
  [points]
  (position-at
    [_ t]
    (let [n (dec (count points))]
      (apply vec/sum
             (for [i (range (inc n))
                   :let [point (nth points i)]]
               (vec/scale (* (binomial n i)
                             (Math/pow t i)
                             (Math/pow (- 1 t) (- n i)))
                          point))))))
