(ns core-async-tutorials.async-episode14
  (:require [clojure.test :refer :all]
            [clojure.core.async :refer [chan <! >! <!! >!! go alt!! timeout
                                        pipeline pipeline-blocking
                                        pipeline-async]
             :as async]))


(let [in (chan 1)
      out (chan 1)
      xform (comp (map inc)
                  (filter odd?))
      af (fn [value c]
           (go
             (<! (timeout (rand-int 1000)))
             (>! c (dec value))
             (>! c (inc value))
             (async/close! c)))]
  #_(pipeline 20 out xform in)
  #_(pipeline-blocking 20 out xform in)
  (pipeline-async 100 out af in)

  (async/onto-chan in (range 100))

  (println (<!! (async/into [] out))))