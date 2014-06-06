
(ns core-async-tutorials.episode2
  (:require [clojure.core.async :as async :refer [<!! >!! go <! >!]]
            [clojure.core.async.impl.protocols :as impl]))


;; Episode 2 pipelines (continued)

(let [c (async/chan 10)]
  (>!! c 42)
  (<!! (async/into [] (async/take 10 (pipeline< [4 inc
                                                 1 inc
                                                 2 (fn [x]
                                                     (async/to-chan (range x)))
                                                 10 pause-rnd
                                                 3 str]
                                                c)))))

(defn pause-rnd [x]
  (go (<! (async/timeout (rand-int 1000)))
      x))

(defn to-proc< [in]
  (let [out (async/chan 1)]
    (async/pipe in out)
    out))

(defn pipe-ext [in out]
  (go (loop []
        (when-some [v (<! in)]
                   (>! out v)
                   (recur)))))

(defn map-ext [in f out]
  (go (loop []
        (when-some [val (<! in)]
                   (let [val (f val)]
                     (cond
                       (or (seq? val)
                           (vector? val)) (do (<! (async/onto-chan out val))
                                              (recur))

                       (extends? impl/ReadPort (class val)) (do (<! (pipe-ext val out))
                                                                (recur))

                       :else (do (>! out val)
                                 (recur))))))))

(defn pipeline< [desc c]
  (let [p (partition 2 desc)]
    (reduce
      (fn [prev-c [n f]]
        (let [out-c (async/chan n)]
          (dotimes [_ n]
            (map-ext prev-c f out-c))
          out-c))
      c
      p)))