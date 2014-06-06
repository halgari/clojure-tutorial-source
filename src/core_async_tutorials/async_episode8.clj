(ns core-async-tutorials.episode8
  (:require [clojure.core.async :as async :refer [<!! >!! go <! >! alts! close! chan timeout thread]]
            [cheshire.core :refer [parse-string]]
            [clojure.core.async.impl.protocols :as impl]
            [org.httpkit.client :as http]
            [clojure.data.xml :as data.xml])
  (:import (org.zeromq ZContext ZMQ))
  (:import (java.util.zip Inflater)))


(defn get-prices [data]
  (let [url (str "http://api.eve-central.com/api/marketstat?usesystem=30000142&typeid="
                 (:typeID data))
        c (chan 1)]
    (println url)
    (http/get url (fn [r] (async/put! c r)))
    c))

(defn promise-c [in-c]
  (let [out-c (chan 1)]
    (go (let [value (<! in-c)]
          (while (>! out-c value))))
    out-c))

(defn cache-fn [f]
  (let [cache (atom {})]
    (fn [& args]
      (if-let [result (get @cache args)]
        result
        (let [ret-c (promise-c (apply f args))]
          (swap! cache assoc args ret-c)
          ret-c)))))

(def cached (cache-fn get-prices))
(<!! (cached {:typeID 34}))


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