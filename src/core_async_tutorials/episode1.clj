(ns core-async-tutorials.episode1
  (:require [clojure.core.async :as async :refer [<!! >!!]]))


;; Episode 1 pipelines

(let [c (async/chan 10)]
  (>!! c 42)
  (<!! (pipeline< [4 inc
                   1 inc
                   2 dec
                   3 str]
                  c)))

(defn to-proc< [in]
  (let [out (async/chan 1)]
    (async/pipe in out)
    out))

(defn pipeline< [desc c]
  (let [p (partition 2 desc)]
    (reduce
      (fn [prev-c [n f]]
        (-> (for [_ (range n)]
              (-> (async/map< f prev-c)
                  to-proc<))
            async/merge))
      c
      p)))