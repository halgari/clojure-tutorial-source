(ns core-async-tutorials.episode6
  (:require [clojure.core.async :as async :refer [<!! >!! go <! >! alts! close! chan timeout thread]]
            [cheshire.core :refer [parse-string]]
            [clojure.core.async.impl.protocols :as impl])
  (:import (org.zeromq ZContext ZMQ))
  (:import (java.util.zip Inflater)))


(defn eve-market-data []
  (let [out (chan 1024)
        context (ZMQ/context 1)
        subscriber (.socket context ZMQ/SUB)]
    (.connect subscriber "tcp://relay-us-central-1.eve-emdr.com:8050")
    (.subscribe subscriber (byte-array 0))
    (thread
      (try
        (loop []
          (let [data (.recv subscriber)]
            (when (>!! out data)
              (recur))))
        (catch Throwable ex
          (println "Error: " ex)))
      (close! out)
      (println "Shutting down")
      (.term context))
    out))

(defn inflater [data]
  (let [inflater (Inflater.)
        decompressed (byte-array (* (alength data) 16))
        _ (.setInput inflater data)
        decompressed-size (.inflate inflater decompressed)
        output (byte-array decompressed-size)]
    (System/arraycopy decompressed 0 output 0 decompressed-size)
    (String. output "UTF-8")))

(let [c (eve-market-data)]
  (println (:rows (first (:rowsets (parse-string (inflater (<!! c)) true)))))
  (close! c))

(let [c (eve-market-data)
      pipe (pipeline< [2 inflater

                       2 #(parse-string % true)

                       1 (comp :rows first :rowsets)

                       1 (to-jmq)]
                      c)]
  (println (<!! (async/into [] (async/take 4 pipe))))
  (close! c))


(defrecord Patch [in proc out])

(defn batcher [ms]
  (let [inner (chan 1)
        in (chan 1)
        proc (go (loop [t (timeout ms)]
                   (let [[v c] (alts! [t in])]
                     (condp identical? c
                       t (do (>! inner ::split)
                             (recur (timeout ms)))
                       in (if-not (nil? v)
                            (do (>! inner v)
                                (recur t))
                            (close! inner))))))
        out (->> (async/partition-by (partial identical? ::split) inner)
                 (async/remove< (comp (partial identical? ::split) first)))]
    (->Patch in proc out)))



(defn sink [f c]
  (go (loop []
        (when-some [v (<! c)]
                   (f v)
                   (recur)))))

(defn wait-100 [v]
  (go (<! (async/timeout 100))
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
        (if (instance? Patch f)
          (let [{:keys [in out]} f]
            (async/pipe prev-c in)
            out)
          (let [out-c (async/chan n)
                procs (for [_ (range n)]
                        (map-ext prev-c f out-c))
                close-chan (async/merge procs)]
            (async/take! close-chan (fn [_] (async/close! out-c)))
            out-c)))
      c
      p)))