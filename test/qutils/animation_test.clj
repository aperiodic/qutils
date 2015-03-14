(ns qutils.animation-test
  (:refer-clojure :exclude [rand-int])
  (:require [clojure.test :refer :all]
            [qutils.animation :refer :all]
            [qutils.curve :as curve]
            [qutils.util :refer [alphabet rand-int]] :reload))

(deftest kw->quantity-test
  ;; this should really be using generative checking, man I'm a scrub
  (let [kw->q #'qutils.animation/kw->quantity
        empty-kw (keyword "")]
    (is (= [10 empty-kw] (kw->q 10)))
    (is (= ['foo empty-kw] (kw->q 'foo)))
    (is (= ['foo2 empty-kw] (kw->q 'foo2)))
    (is (= [10 :s] (kw->q (symbol "10s"))))
    (is (= [:10 empty-kw] (kw->q :10)))
    (is (= [:bar empty-kw] (kw->q :bar)))
    (dotimes [_ 1e5]
      (let [q (rand-int (Math/pow 2 20))
            u (->> (shuffle alphabet)
                (take (rand-int 1 5))
                (apply str))
            kw (-> (str q u) keyword)]
        (is (= [q (keyword u)] (kw->q kw)))))))

(deftest animation-test
  (let [id (curve/linear 0 1)]
    (is (thrown? IllegalArgumentException (animation id :0s :1d)))
    (is (animation id 0 :1d))
    (is (animation id :1d 0))
    (doseq [id-anim [(animation id 0 1) (animation id :0s :1s)]
            i (range 0 1 0.01)]
      (is (= i (id-anim i) (apply id-anim [i]))))))
