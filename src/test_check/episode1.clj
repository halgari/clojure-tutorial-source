(ns test-check.episode1
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :refer [quick-check]]))


(def commands-gen (gen/vector (gen/tuple
                                (gen/elements [:+ :-])
                                (gen/choose 0 1000))))

(defn run-correct [commands]
  (reduce
    (fn [acc [op val]]
      (if (= :+ op)
        (+ acc val)
        (- acc val)))
    0
    commands))

(defn run-incorrect [commands]
  (reduce
    (fn [acc [op val]]
      (if (= val 0)
        0
        (if (= :+ op)
          (+ acc val)
          (- acc val))))
    0
    commands))

(quick-check
  100
  (prop/for-all [cmds commands-gen]
                (assert (= (run-incorrect cmds) (run-correct cmds)))
                true))