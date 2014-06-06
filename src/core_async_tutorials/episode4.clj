(ns core-async-tutorials.episode4
  (:require [clojure.core.async :as async :refer [<!! >!! go <! >!]]
            [clojure.core.async.impl.protocols :as impl]))


;; Episode 4 - shutdowns

(let [c (async/chan 1024)]
  (>!! c 42)
  (sink println (pipeline< [4 inc
                            2 range
                            3 inc
                            3 wait-100
                            2 str]
                           c))
  (async/close! c))

(defn sink [f c]
  (go (loop []
        (when-some [v (<! c)]
                   (f v)
                   (recur)))))

(defn wait-100 [v]
  (go (<! (async/timeout 1000))
      v))

(def log-c (async/chan 1024))
(go (loop []
      (println "Got log item: " (<! log-c))
      (recur)))

(defn pipe-ext [in out]
  (go (loop []
        (when-some [v (<! in)]
                   (>! out v)
                   (recur)))))

(defn map-ext [in f out]
  (go (loop []
        (when-some [val (<! in)]
                   (let [val (try (f val)
                                  (catch Throwable ex
                                    (>! log-c ["Failure: " ex f val])
                                    :exception))]
                     (cond
                       (or (seq? val)
                           (vector? val)) (do (<! (async/onto-chan out val))
                                              (recur))

                       (extends? impl/ReadPort (class val)) (do (<! (pipe-ext val out))
                                                                (recur))
                       (identical? val :exception) (recur)
                       (nil? val) (recur)
                       :else (do (>! out val)
                                 (recur))))))
      nil))

(defn pipeline< [desc c]
  (let [p (partition 2 desc)]
    (reduce
      (fn [prev-c [n f]]
        (let [out-c (async/chan n)
              procs (for [_ (range n)]
                      (map-ext prev-c f out-c))
              close-chan (async/merge procs)]
          (async/take! close-chan (fn [_] (async/close! out-c)))
          out-c))
      c
      p)))