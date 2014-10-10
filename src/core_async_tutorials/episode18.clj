(ns core-async-tutorials.episode18
  (:require [clojure.core.async :as async
             :refer [go <! >! <!! >!! put! take!]]))


(let [c (async/chan 1)]
  (go (println (<! c)))
  (put! c 42))

(let [c (async/chan 1)]
  (take! c (fn [v] (println v)))
  (put! c 42))


(let [c (async/chan 1)]
  (go (dotimes [x 3]
        (<! c))
      (println "done"))
  (put! c 42)
  (put! c 42)
  (put! c 42))

(let [c (async/chan 1)]
  (let [a (atom 0)
        f (fn f []
            (take! c (fn [_]
                       (if (= (swap! a inc) 3)
                         (println "done")
                         (f)))))]
    (f))
  (put! c 42)
  (put! c 42)
  (put! c 42))

(let [c (async/chan 1)]
  (go (doall (map <! [c c c]))))

(macroexpand '(dotimes [x 10] (<! c))))

(go
  (let [c (async/chan 1)
        a (atom 0)]
    (swap! a + (<! c))))





