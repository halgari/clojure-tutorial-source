(ns core-async-tutorials.episode19
  (:require [clojure.core.async :refer [go <! >! alts!! close! take! put! >!! <!!
                                        chan timeout]]))


(defn take-all [& chans]
  (loop [acc []]
    (let [[v c] (alts!! (vec chans) :priority true :default nil)]
      (if (not= c :default)
        (recur (conj acc v))
        acc))))

(let [a (chan 10)
      b (chan 10)
      c (chan 10)]
  (>!! a 0)
  (>!! a 1)
  (>!! b 2)
  (>!! b 3)
  (>!! c 4)

  (println (take-all a b c)))

