(ns qutils.scratch
  (:require [qutils.animation :as anim]
            [qutils.curve :as curve]
            [qutils.util :refer [interpolate]] :reload))

(comment

  (let [id (curve/linear 0 1)
        id-anim (anim/animation id 0 1)]

    ;; curves and animations can both be used as functions
    (id 0.5)
    ;; => 0.5
    (id-anim 0.75)
    ;; => 0.75

    ;; Curves are functions of t, and are only guaranteed to work from 0 to 1.
    ;; They may work outside of that range, as linear functions do.
    (id -1)
    ;; => -1

    ;; Animations, however, are clamped to the values of the curve at t = 0 and t = 1.
    (id-anim -1))
    ;; => 0

  )
