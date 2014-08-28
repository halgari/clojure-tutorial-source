(ns zippers.zippers2
  (:require [clojure.zip :as z]))

(def data [1
           [2 3 4]
           [[5 6]
            [7 8]
            [[9 10 11]]]
           12])

(def data {:op :if
           :children [:test :then :else]
           :test {:op :eq
                  :children [:a :b]
                  :a {:op :const
                      :val 42}
                  :b {:op :const
                      :val 42}}
           :then {:op :const
                  :val "true"}
           :else {:op :const
                  :val "false"}})

(defn ast-zip [data]
  (z/zipper :children
            (fn [node]
              (map node (:children node)))
            (fn [old new]
              (merge old
                     (zipmap (:children old)
                             new)))
            data))

(defn vec-zip [data]
  (z/zipper vector?
            seq
            (fn [old new]
              (vec new))
            data))

(ast-zip data)

(-> (ast-zip data)
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

(zip-map (fn [n]
           (println n)
           n)
         (ast-zip data))


