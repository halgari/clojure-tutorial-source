(ns core-async-tutorials.episode9
  (:require [clojure.core.async :as async :refer [<!! >!! go <! >! alts! close! chan timeout thread]]
            [cheshire.core :refer [parse-string]]
            [clojure.core.async.impl.protocols :as impl]
            [org.httpkit.client :as http]
            [clojure.data.xml :as data.xml])
  (:import (org.zeromq ZContext ZMQ))
  (:import (java.util.zip Inflater)))


(defn map-proc
  ([in f out]
   (map-proc in f out num))
  ([in f out num]
   (doall (for [x (range num)]
            (go (loop []
                  (when-some [v (<! in)]
                             (>! (f out))
                             (recur))))))))

(defn split [in pred out1 out2]
  (go (loop []
        (when-some [v (<! in)]
                   (if (pred v)
                     (>! out1)
                     (>! out2))
                   (recur)))))

(let [step1 (chan 1)
      even (chan 1)
      odd (chan 1)
      sink (chan (dropping-buffer 1))]
  (split step1 even? even odd)
  (map-proc even #(println "got even: " %) sink 2)
  (map-proc odd #(println "got odd: " %) sink 4)
  #_(async/pipe (async/merge even odd)
              sink))