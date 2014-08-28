(ns zippers.zippers1
  (:require [clojure.zip :as z]))

(def data [1
           [2 3 4]
           [[5 6]
            [7 8]
            [[9 10 11]]]
           12])

(-> (z/vector-zip data)
    (z/next)
    (z/next)
    (z/next)
    (z/next)
    (z/node))


(defn zip-map [f z]
  (loop [z z]
    (if (identical? (z/next z) z)
      (z/root z)
      (if (z/branch? z)
        (recur (z/next z))
        (recur (-> z (z/edit f) z/next))))))

(zip-map inc (z/vector-zip data))
