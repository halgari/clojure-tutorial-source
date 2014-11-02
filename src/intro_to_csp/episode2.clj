(ns intro-to-csp.episode2
  (:require [clojure.core.async :as a]))


(def c (a/chan))
(def a (a/chan))

(a/put! c "hello" (fn [_] (println "Done sending")))

(a/take! c (fn [val] (println val)))


(a/go
  (a/>! c "Hello")
  (a/>! c "World"))

(a/go
  (println (a/<! c))
  (println (a/<! c)))



(a/go
  (println (a/alts! [c a (a/timeout 1000)])))

(println c)


(defn buffer [from]
  (let [ret (a/chan)]
    (a/go
      (while true
        (a/>! ret (a/<! from))))
    ret))

(def c (a/chan 10))
(def b (buffer c))
(def a (buffer b))

(a/put! c "hello" (fn [_] (println "Done sending")))

(a/take! a (fn [val] (println val)))